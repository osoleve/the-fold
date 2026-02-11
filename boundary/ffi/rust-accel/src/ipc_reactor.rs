//! IPC Reactor — Unix domain socket server with epoll-based IO
//!
//! Runs on a background thread. Scheme calls poll/send through FFI.
//!
//! Architecture:
//!   Rust background thread: epoll on listener + clients, frame buffering
//!   Scheme main thread: ipc_poll() to pop frames, ipc_send() to enqueue
//!
//! Frame format: 4-byte big-endian length prefix + UTF-8 payload

use crate::posix::StatusResult;
use std::collections::VecDeque;
use std::io::{Read, Write};
use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};
use std::os::unix::net::{UnixListener, UnixStream};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

// ====
// Constants
// ====

/// Maximum frame payload size: 16MB
const MAX_FRAME_SIZE: u32 = 16 * 1024 * 1024;

/// Frame header size: 4 bytes (u32 big-endian)
const HEADER_SIZE: usize = 4;

/// Maximum concurrent client connections
const MAX_CLIENTS: usize = 256;

/// epoll read buffer size per recv call
const READ_BUF_SIZE: usize = 65536;

/// epoll timeout in milliseconds (how often the thread checks for shutdown)
const EPOLL_TIMEOUT_MS: i32 = 100;

// ====
// Per-Connection State
// ====

/// Accumulates bytes for a single connection until a complete frame is ready.
struct ConnectionBuffer {
    /// Client ID for routing
    client_id: u64,
    /// Raw fd for the connection
    fd: RawFd,
    /// Accumulated bytes (may contain partial frame)
    buf: Vec<u8>,
    /// Outbound queue (frames to send to this client)
    outbound: VecDeque<Vec<u8>>,
    /// Offset into the current outbound frame being written
    out_offset: usize,
}

impl ConnectionBuffer {
    fn new(client_id: u64, fd: RawFd) -> Self {
        Self {
            client_id,
            fd,
            buf: Vec::with_capacity(4096),
            outbound: VecDeque::new(),
            out_offset: 0,
        }
    }

    /// Try to extract complete frames from the buffer.
    /// Returns a vec of (client_id, payload_bytes) for each complete frame.
    fn extract_frames(&mut self) -> Vec<(u64, Vec<u8>)> {
        let mut frames = Vec::new();
        loop {
            if self.buf.len() < HEADER_SIZE {
                break;
            }
            let payload_len =
                u32::from_be_bytes([self.buf[0], self.buf[1], self.buf[2], self.buf[3]]) as usize;

            if payload_len > MAX_FRAME_SIZE as usize {
                // Malformed: discard connection data
                self.buf.clear();
                break;
            }

            let frame_len = HEADER_SIZE + payload_len;
            if self.buf.len() < frame_len {
                break; // Incomplete frame, wait for more data
            }

            // Extract payload (skip header)
            let payload = self.buf[HEADER_SIZE..frame_len].to_vec();
            frames.push((self.client_id, payload));

            // Remove consumed bytes
            self.buf.drain(..frame_len);
        }
        frames
    }
}

// ====
// Inbound Message (from client to Scheme)
// ====

struct InboundMessage {
    client_id: u64,
    payload: Vec<u8>,
}

// ====
// Reactor State (shared between background thread and FFI calls)
// ====

struct ReactorState {
    /// Queue of complete inbound frames for Scheme to poll
    inbound: Mutex<VecDeque<InboundMessage>>,
    /// Whether the reactor should keep running
    running: AtomicBool,
    /// Next client ID to assign
    next_client_id: AtomicU64,
    /// Number of connected clients
    client_count: AtomicU64,
    /// Outbound frames to enqueue (client_id, frame_with_header)
    /// The background thread drains this into per-connection outbound queues
    outbound: Mutex<VecDeque<(u64, Vec<u8>)>>,
}

impl ReactorState {
    fn new() -> Self {
        Self {
            inbound: Mutex::new(VecDeque::new()),
            running: AtomicBool::new(true),
            next_client_id: AtomicU64::new(1),
            client_count: AtomicU64::new(0),
            outbound: Mutex::new(VecDeque::new()),
        }
    }
}

// ====
// Global Reactor Handle
// ====

static REACTOR: Mutex<Option<Arc<ReactorState>>> = Mutex::new(None);
static REACTOR_THREAD: Mutex<Option<thread::JoinHandle<()>>> = Mutex::new(None);

// ====
// Background Thread
// ====

fn reactor_thread(listener: UnixListener, state: Arc<ReactorState>) {
    let epfd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
    if epfd < 0 {
        return;
    }

    // Add listener to epoll
    let listener_fd = listener.as_raw_fd();
    let mut ev = libc::epoll_event {
        events: libc::EPOLLIN as u32,
        u64: listener_fd as u64,
    };
    unsafe {
        libc::epoll_ctl(epfd, libc::EPOLL_CTL_ADD, listener_fd, &mut ev);
    }

    // Per-connection buffers, keyed by fd
    let mut connections: Vec<ConnectionBuffer> = Vec::new();
    let mut events = vec![libc::epoll_event { events: 0, u64: 0 }; MAX_CLIENTS + 1];
    let mut read_buf = vec![0u8; READ_BUF_SIZE];

    while state.running.load(Ordering::Relaxed) {
        // Drain outbound queue into per-connection queues
        {
            if let Ok(mut outq) = state.outbound.lock() {
                while let Some((client_id, frame)) = outq.pop_front() {
                    if let Some(conn) = connections.iter_mut().find(|c| c.client_id == client_id) {
                        conn.outbound.push_back(frame);
                        // Mark fd for EPOLLOUT
                        let mut ev = libc::epoll_event {
                            events: (libc::EPOLLIN | libc::EPOLLOUT) as u32,
                            u64: conn.fd as u64,
                        };
                        unsafe {
                            libc::epoll_ctl(epfd, libc::EPOLL_CTL_MOD, conn.fd, &mut ev);
                        }
                    }
                }
            }
        }

        let nfds = unsafe {
            libc::epoll_wait(
                epfd,
                events.as_mut_ptr(),
                events.len() as i32,
                EPOLL_TIMEOUT_MS,
            )
        };

        if nfds < 0 {
            let errno = unsafe { *libc::__errno_location() };
            if errno == libc::EINTR {
                continue;
            }
            break;
        }

        for i in 0..nfds as usize {
            let fd = events[i].u64 as RawFd;
            let event_flags = events[i].events;

            if fd == listener_fd {
                // Accept new connection
                match listener.accept() {
                    Ok((stream, _addr)) => {
                        if connections.len() >= MAX_CLIENTS {
                            drop(stream);
                            continue;
                        }

                        let client_fd = stream.as_raw_fd();
                        // Set non-blocking
                        unsafe {
                            let flags = libc::fcntl(client_fd, libc::F_GETFL);
                            libc::fcntl(client_fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
                        }

                        let client_id = state.next_client_id.fetch_add(1, Ordering::Relaxed);
                        state.client_count.fetch_add(1, Ordering::Relaxed);

                        // Add to epoll
                        let mut ev = libc::epoll_event {
                            events: libc::EPOLLIN as u32,
                            u64: client_fd as u64,
                        };
                        unsafe {
                            libc::epoll_ctl(epfd, libc::EPOLL_CTL_ADD, client_fd, &mut ev);
                        }

                        connections.push(ConnectionBuffer::new(client_id, client_fd));

                        // Don't drop the stream — we manage the fd directly
                        std::mem::forget(stream);
                    }
                    Err(_) => continue,
                }
            } else {
                // Existing client
                let conn_idx = connections.iter().position(|c| c.fd == fd);
                if conn_idx.is_none() {
                    continue;
                }
                let idx = conn_idx.unwrap();

                let mut should_remove = false;

                // Handle readable
                if event_flags & libc::EPOLLIN as u32 != 0 {
                    let n = unsafe {
                        libc::recv(
                            fd,
                            read_buf.as_mut_ptr() as *mut libc::c_void,
                            read_buf.len(),
                            0,
                        )
                    };

                    if n <= 0 {
                        should_remove = true;
                    } else {
                        connections[idx]
                            .buf
                            .extend_from_slice(&read_buf[..n as usize]);
                        let frames = connections[idx].extract_frames();
                        if !frames.is_empty() {
                            if let Ok(mut inq) = state.inbound.lock() {
                                for (cid, payload) in frames {
                                    inq.push_back(InboundMessage {
                                        client_id: cid,
                                        payload,
                                    });
                                }
                            }
                        }
                    }
                }

                // Handle writable
                if !should_remove && event_flags & libc::EPOLLOUT as u32 != 0 {
                    let conn = &mut connections[idx];
                    while !conn.outbound.is_empty() {
                        let frame = &conn.outbound[0];
                        let remaining = &frame[conn.out_offset..];
                        let n = unsafe {
                            libc::send(
                                fd,
                                remaining.as_ptr() as *const libc::c_void,
                                remaining.len(),
                                libc::MSG_NOSIGNAL,
                            )
                        };
                        if n < 0 {
                            let errno = unsafe { *libc::__errno_location() };
                            if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK {
                                break; // Try again on next epoll cycle
                            }
                            should_remove = true;
                            break;
                        } else if n == 0 {
                            should_remove = true;
                            break;
                        } else {
                            conn.out_offset += n as usize;
                            if conn.out_offset >= conn.outbound[0].len() {
                                conn.outbound.pop_front();
                                conn.out_offset = 0;
                            }
                        }
                    }

                    // If outbound drained, stop watching for EPOLLOUT
                    if !should_remove && conn.outbound.is_empty() {
                        let mut ev = libc::epoll_event {
                            events: libc::EPOLLIN as u32,
                            u64: fd as u64,
                        };
                        unsafe {
                            libc::epoll_ctl(epfd, libc::EPOLL_CTL_MOD, fd, &mut ev);
                        }
                    }
                }

                // Handle error/hangup
                if event_flags & (libc::EPOLLERR | libc::EPOLLHUP) as u32 != 0 {
                    should_remove = true;
                }

                if should_remove {
                    unsafe {
                        libc::epoll_ctl(epfd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut());
                        libc::close(fd);
                    }
                    state.client_count.fetch_sub(1, Ordering::Relaxed);
                    connections.remove(idx);
                }
            }
        }
    }

    // Cleanup: close all client connections
    for conn in &connections {
        unsafe {
            libc::epoll_ctl(epfd, libc::EPOLL_CTL_DEL, conn.fd, std::ptr::null_mut());
            libc::close(conn.fd);
        }
    }
    unsafe {
        libc::close(epfd);
    }
}

// ====
// FFI: Server Lifecycle
// ====

/// Start the IPC reactor on the given Unix domain socket path.
///
/// # Safety
/// Caller must provide valid path pointer and status out-pointer.
#[no_mangle]
pub extern "C" fn fold_ipc_start(path_ptr: *const u8, path_len: usize, out: *mut StatusResult) {
    if out.is_null() || path_ptr.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Check if already running
    let mut reactor_guard = match REACTOR.lock() {
        Ok(g) => g,
        Err(_) => {
            result.status = 1;
            result.error_code = libc::EIO;
            return;
        }
    };
    if reactor_guard.is_some() {
        result.status = 1;
        result.error_code = libc::EEXIST;
        return;
    }

    let path_slice = unsafe { std::slice::from_raw_parts(path_ptr, path_len) };
    let path_str = match std::str::from_utf8(path_slice) {
        Ok(s) => s.to_string(),
        Err(_) => {
            result.status = 1;
            result.error_code = libc::EINVAL;
            return;
        }
    };

    // Remove stale socket file if it exists
    let _ = std::fs::remove_file(&path_str);

    // Bind listener
    let listener = match UnixListener::bind(&path_str) {
        Ok(l) => l,
        Err(e) => {
            result.status = 1;
            result.error_code = e.raw_os_error().unwrap_or(libc::EIO);
            return;
        }
    };

    // Restrict socket permissions to owner only (0600)
    unsafe {
        let path_c = std::ffi::CString::new(path_str.as_bytes()).unwrap();
        libc::chmod(path_c.as_ptr(), 0o600);
    }

    // Set listener non-blocking
    listener.set_nonblocking(true).ok();

    let state = Arc::new(ReactorState::new());
    let state_clone = Arc::clone(&state);

    let handle = thread::spawn(move || {
        reactor_thread(listener, state_clone);
    });

    *reactor_guard = Some(state);
    if let Ok(mut thread_guard) = REACTOR_THREAD.lock() {
        *thread_guard = Some(handle);
    }

    result.status = 0;
    result.error_code = 0;
}

/// Stop the IPC reactor and clean up.
///
/// # Safety
/// Caller must provide valid status out-pointer.
#[no_mangle]
pub extern "C" fn fold_ipc_stop(out: *mut StatusResult) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Signal shutdown
    if let Ok(guard) = REACTOR.lock() {
        if let Some(ref state) = *guard {
            state.running.store(false, Ordering::Relaxed);
        }
    }

    // Join the thread (must drop reactor lock first to avoid deadlock)
    if let Ok(mut thread_guard) = REACTOR_THREAD.lock() {
        if let Some(handle) = thread_guard.take() {
            let _ = handle.join();
        }
    }

    // Clear reactor state
    if let Ok(mut guard) = REACTOR.lock() {
        *guard = None;
    }

    result.status = 0;
    result.error_code = 0;
}

// ====
// FFI: Inbound (server reads from clients)
// ====

/// Poll for the next complete inbound message.
///
/// Returns status 0 if a message was available (out_client_id and buffer filled),
/// status 1 if the queue is empty, status 2 on error.
///
/// # Safety
/// Caller must provide valid pointers and ensure buffer capacity is correct.
#[no_mangle]
pub extern "C" fn fold_ipc_poll(
    out_client_id: *mut u64,
    out_buf: *mut u8,
    buf_capacity: usize,
    out_len: *mut usize,
    out: *mut StatusResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let state = match REACTOR.lock() {
        Ok(guard) => match guard.as_ref() {
            Some(s) => Arc::clone(s),
            None => {
                result.status = 2;
                result.error_code = libc::ENOENT;
                return;
            }
        },
        Err(_) => {
            result.status = 2;
            result.error_code = libc::EIO;
            return;
        }
    };

    let msg = {
        match state.inbound.lock() {
            Ok(mut q) => q.pop_front(),
            Err(_) => {
                result.status = 2;
                result.error_code = libc::EIO;
                return;
            }
        }
    };

    match msg {
        Some(m) => {
            if m.payload.len() > buf_capacity {
                // Buffer too small — put message back
                if let Ok(mut q) = state.inbound.lock() {
                    q.push_front(m);
                }
                result.status = 2;
                result.error_code = libc::ENOSPC;
                return;
            }

            unsafe {
                if !out_client_id.is_null() {
                    *out_client_id = m.client_id;
                }
                if !out_buf.is_null() {
                    std::ptr::copy_nonoverlapping(m.payload.as_ptr(), out_buf, m.payload.len());
                }
                if !out_len.is_null() {
                    *out_len = m.payload.len();
                }
            }
            result.status = 0;
            result.error_code = 0;
        }
        None => {
            // Queue empty
            result.status = 1;
            result.error_code = 0;
        }
    }
}

// ====
// FFI: Outbound (server sends to clients)
// ====

/// Send a length-prefixed frame to a specific client.
/// The frame bytes should include the 4-byte length header.
///
/// # Safety
/// Caller must provide valid buffer pointer.
#[no_mangle]
pub extern "C" fn fold_ipc_send(
    client_id: u64,
    buf: *const u8,
    len: usize,
    out: *mut StatusResult,
) {
    if out.is_null() || buf.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let state = match REACTOR.lock() {
        Ok(guard) => match guard.as_ref() {
            Some(s) => Arc::clone(s),
            None => {
                result.status = 1;
                result.error_code = libc::ENOENT;
                return;
            }
        },
        Err(_) => {
            result.status = 1;
            result.error_code = libc::EIO;
            return;
        }
    };

    let frame = unsafe { std::slice::from_raw_parts(buf, len).to_vec() };

    match state.outbound.lock() {
        Ok(mut q) => {
            q.push_back((client_id, frame));
            result.status = 0;
            result.error_code = 0;
        }
        Err(_) => {
            result.status = 1;
            result.error_code = libc::EIO;
        }
    };
}

/// Get the current number of connected clients.
#[no_mangle]
pub extern "C" fn fold_ipc_client_count() -> u64 {
    match REACTOR.lock() {
        Ok(guard) => match guard.as_ref() {
            Some(s) => s.client_count.load(Ordering::Relaxed),
            None => 0,
        },
        Err(_) => 0,
    }
}

// ====
// FFI: Client-side (for Scheme processes connecting TO the daemon)
// ====

/// Connect to a Unix domain socket as a client.
/// Returns the fd in IntResult on success.
///
/// # Safety
/// Caller must provide valid path and result pointers.
#[no_mangle]
pub extern "C" fn fold_ipc_connect(
    path_ptr: *const u8,
    path_len: usize,
    out: *mut crate::posix::IntResult,
) {
    if out.is_null() || path_ptr.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let path_slice = unsafe { std::slice::from_raw_parts(path_ptr, path_len) };
    let path_str = match std::str::from_utf8(path_slice) {
        Ok(s) => s,
        Err(_) => {
            result.status = 1;
            result.value = -1;
            result.error_code = libc::EINVAL;
            return;
        }
    };

    match UnixStream::connect(path_str) {
        Ok(stream) => {
            let fd = stream.as_raw_fd();
            std::mem::forget(stream); // Caller manages fd lifetime
            result.status = 0;
            result.value = fd;
            result.error_code = 0;
        }
        Err(e) => {
            result.status = 1;
            result.value = -1;
            result.error_code = e.raw_os_error().unwrap_or(libc::EIO);
        }
    }
}

/// Send a complete frame (with length header) to the daemon.
///
/// # Safety
/// Caller must provide valid fd, buffer pointer, and result pointer.
#[no_mangle]
pub extern "C" fn fold_ipc_client_send(
    fd: i32,
    buf: *const u8,
    len: usize,
    out: *mut StatusResult,
) {
    if out.is_null() || buf.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let data = unsafe { std::slice::from_raw_parts(buf, len) };
    let mut stream = unsafe { UnixStream::from_raw_fd(fd) };
    match stream.write_all(data) {
        Ok(_) => {
            result.status = 0;
            result.error_code = 0;
        }
        Err(e) => {
            result.status = 1;
            result.error_code = e.raw_os_error().unwrap_or(libc::EIO);
        }
    }
    // Don't drop the stream — caller still owns the fd
    std::mem::forget(stream);
}

/// Receive a complete frame from the daemon (blocking with timeout).
/// Reads the 4-byte length header, then reads exactly that many payload bytes.
/// Writes the payload (without header) to out_buf.
///
/// # Safety
/// Caller must provide valid fd, buffer pointer, and result pointer.
#[no_mangle]
pub extern "C" fn fold_ipc_client_recv(
    fd: i32,
    out_buf: *mut u8,
    capacity: usize,
    out_len: *mut usize,
    out: *mut StatusResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let mut stream = unsafe { UnixStream::from_raw_fd(fd) };

    // Set read timeout: 120 seconds
    stream
        .set_read_timeout(Some(std::time::Duration::from_secs(120)))
        .ok();

    // Read 4-byte length header
    let mut header = [0u8; HEADER_SIZE];
    match stream.read_exact(&mut header) {
        Ok(_) => {}
        Err(e) => {
            result.status = 1;
            result.error_code = e.raw_os_error().unwrap_or(libc::EIO);
            std::mem::forget(stream);
            return;
        }
    }

    let payload_len = u32::from_be_bytes(header) as usize;
    if payload_len > MAX_FRAME_SIZE as usize || payload_len > capacity {
        result.status = 1;
        result.error_code = libc::ENOSPC;
        std::mem::forget(stream);
        return;
    }

    // Read payload
    let mut payload_buf = vec![0u8; payload_len];
    match stream.read_exact(&mut payload_buf) {
        Ok(_) => {
            unsafe {
                if !out_buf.is_null() {
                    std::ptr::copy_nonoverlapping(payload_buf.as_ptr(), out_buf, payload_len);
                }
                if !out_len.is_null() {
                    *out_len = payload_len;
                }
            }
            result.status = 0;
            result.error_code = 0;
        }
        Err(e) => {
            result.status = 1;
            result.error_code = e.raw_os_error().unwrap_or(libc::EIO);
        }
    }

    std::mem::forget(stream);
}

/// Close a client connection fd.
///
/// # Safety
/// Caller must provide valid fd.
#[no_mangle]
pub extern "C" fn fold_ipc_client_close(fd: i32, out: *mut StatusResult) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };
    let ret = unsafe { libc::close(fd) };
    if ret < 0 {
        result.status = 1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.error_code = 0;
    }
}

// ====
// Tests
// ====

#[cfg(test)]
mod tests {
    use super::*;

    /// Serializes tests that use the global REACTOR singleton.
    /// cargo test runs in parallel; without this, tests race on start/stop.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    /// Counter to generate unique socket paths per test (PID alone isn't enough
    /// when tests run sequentially within the same process).
    static TEST_COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

    fn temp_sock_path() -> String {
        let n = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
        format!("/tmp/fold-ipc-test-{}-{}.sock", std::process::id(), n)
    }

    /// Ensure reactor is stopped before/after test (defensive cleanup).
    fn ensure_reactor_stopped() {
        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_ipc_stop(&mut status);
    }

    #[test]
    fn test_connection_buffer_single_frame() {
        let mut buf = ConnectionBuffer::new(1, -1);
        // Write a frame: length=5, payload="hello"
        let payload = b"hello";
        let len_bytes = (payload.len() as u32).to_be_bytes();
        buf.buf.extend_from_slice(&len_bytes);
        buf.buf.extend_from_slice(payload);

        let frames = buf.extract_frames();
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].0, 1); // client_id
        assert_eq!(frames[0].1, b"hello");
        assert!(buf.buf.is_empty());
    }

    #[test]
    fn test_connection_buffer_partial_header() {
        let mut buf = ConnectionBuffer::new(1, -1);
        buf.buf.extend_from_slice(&[0, 0]); // Only 2 of 4 header bytes
        let frames = buf.extract_frames();
        assert!(frames.is_empty());
        assert_eq!(buf.buf.len(), 2); // Bytes preserved
    }

    #[test]
    fn test_connection_buffer_partial_payload() {
        let mut buf = ConnectionBuffer::new(1, -1);
        let len_bytes = 10u32.to_be_bytes();
        buf.buf.extend_from_slice(&len_bytes);
        buf.buf.extend_from_slice(b"short"); // Only 5 of 10 bytes
        let frames = buf.extract_frames();
        assert!(frames.is_empty());
        assert_eq!(buf.buf.len(), 9); // 4 header + 5 data preserved
    }

    #[test]
    fn test_connection_buffer_multiple_frames() {
        let mut buf = ConnectionBuffer::new(1, -1);
        for msg in &["abc", "defgh"] {
            let payload = msg.as_bytes();
            let len_bytes = (payload.len() as u32).to_be_bytes();
            buf.buf.extend_from_slice(&len_bytes);
            buf.buf.extend_from_slice(payload);
        }

        let frames = buf.extract_frames();
        assert_eq!(frames.len(), 2);
        assert_eq!(frames[0].1, b"abc");
        assert_eq!(frames[1].1, b"defgh");
    }

    #[test]
    fn test_connection_buffer_oversized_frame() {
        let mut buf = ConnectionBuffer::new(1, -1);
        // Claim a payload larger than MAX_FRAME_SIZE
        let bad_len = (MAX_FRAME_SIZE + 1).to_be_bytes();
        buf.buf.extend_from_slice(&bad_len);
        buf.buf.extend_from_slice(b"x");
        let frames = buf.extract_frames();
        assert!(frames.is_empty());
        assert!(buf.buf.is_empty()); // Buffer cleared (malformed)
    }

    #[test]
    fn test_start_stop_reactor() {
        let _lock = TEST_LOCK.lock().unwrap();
        ensure_reactor_stopped();

        let path = temp_sock_path();
        let path_bytes = path.as_bytes();

        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        // Start
        fold_ipc_start(path_bytes.as_ptr(), path_bytes.len(), &mut status);
        assert_eq!(
            status.status, 0,
            "start failed: errno={}",
            status.error_code
        );

        // Verify we can connect
        assert!(std::path::Path::new(&path).exists());
        assert_eq!(fold_ipc_client_count(), 0);

        // Stop
        fold_ipc_stop(&mut status);
        assert_eq!(status.status, 0);

        // Cleanup
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn test_client_connect_send_recv() {
        let _lock = TEST_LOCK.lock().unwrap();
        ensure_reactor_stopped();

        let path = temp_sock_path();
        let path_bytes = path.as_bytes();

        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        // Start reactor
        fold_ipc_start(path_bytes.as_ptr(), path_bytes.len(), &mut status);
        assert_eq!(status.status, 0);

        // Give reactor thread time to start
        thread::sleep(std::time::Duration::from_millis(50));

        // Connect as client
        let mut connect_result = crate::posix::IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };
        fold_ipc_connect(path_bytes.as_ptr(), path_bytes.len(), &mut connect_result);
        assert_eq!(connect_result.status, 0, "connect failed");
        let client_fd = connect_result.value;

        // Give reactor time to accept
        thread::sleep(std::time::Duration::from_millis(50));
        assert_eq!(fold_ipc_client_count(), 1);

        // Send a frame from client
        let payload = b"((type . ping))";
        let mut frame = Vec::new();
        frame.extend_from_slice(&(payload.len() as u32).to_be_bytes());
        frame.extend_from_slice(payload);

        fold_ipc_client_send(client_fd, frame.as_ptr(), frame.len(), &mut status);
        assert_eq!(status.status, 0);

        // Give reactor time to process
        thread::sleep(std::time::Duration::from_millis(50));

        // Poll for the message on server side
        let mut out_client_id: u64 = 0;
        let mut out_buf = vec![0u8; 4096];
        let mut out_len: usize = 0;

        fold_ipc_poll(
            &mut out_client_id,
            out_buf.as_mut_ptr(),
            out_buf.len(),
            &mut out_len,
            &mut status,
        );
        assert_eq!(status.status, 0, "poll failed: status={}", status.status);
        assert!(out_client_id > 0);
        assert_eq!(&out_buf[..out_len], payload);

        // Send response back via server
        let resp_payload = b"((type . pong))";
        let mut resp_frame = Vec::new();
        resp_frame.extend_from_slice(&(resp_payload.len() as u32).to_be_bytes());
        resp_frame.extend_from_slice(resp_payload);

        fold_ipc_send(
            out_client_id,
            resp_frame.as_ptr(),
            resp_frame.len(),
            &mut status,
        );
        assert_eq!(status.status, 0);

        // Give reactor time to send
        thread::sleep(std::time::Duration::from_millis(50));

        // Receive on client side
        let mut recv_buf = vec![0u8; 4096];
        let mut recv_len: usize = 0;
        fold_ipc_client_recv(
            client_fd,
            recv_buf.as_mut_ptr(),
            recv_buf.len(),
            &mut recv_len,
            &mut status,
        );
        assert_eq!(status.status, 0, "recv failed");
        assert_eq!(&recv_buf[..recv_len], resp_payload);

        // Cleanup
        fold_ipc_client_close(client_fd, &mut status);
        fold_ipc_stop(&mut status);
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn test_poll_empty() {
        let _lock = TEST_LOCK.lock().unwrap();
        ensure_reactor_stopped();

        let path = temp_sock_path();
        let path_bytes = path.as_bytes();

        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        fold_ipc_start(path_bytes.as_ptr(), path_bytes.len(), &mut status);
        assert_eq!(status.status, 0);

        // Poll with no clients connected
        let mut cid: u64 = 0;
        let mut buf = vec![0u8; 1024];
        let mut len: usize = 0;
        fold_ipc_poll(&mut cid, buf.as_mut_ptr(), buf.len(), &mut len, &mut status);
        assert_eq!(status.status, 1); // Empty queue

        fold_ipc_stop(&mut status);
        let _ = std::fs::remove_file(&path);
    }
}
