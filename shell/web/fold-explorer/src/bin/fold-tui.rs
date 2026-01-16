//! Fold TUI - Terminal User Interface for CAS exploration.
//!
//! Usage:
//!   fold-tui [store-path]          - Interactive TUI mode
//!   fold-tui [store-path] --check  - Verify store and print stats
//!
//! Keys (interactive mode):
//!   j/Down  - Move down
//!   k/Up    - Move up
//!   Enter   - View block details / follow ref
//!   b/Esc   - Back to list
//!   /       - Search
//!   t       - Filter by tag
//!   o       - Show orphans
//!   p       - Show popular (most referenced)
//!   h       - Show heads (named references)
//!   r       - Reset filters
//!   q       - Quit

use std::env;
use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::path::PathBuf;
use std::process;

use fold_explorer::store::{BlockMeta, Head, Store};

// Raw terminal control without libc crate (zero-dependency)
mod term {
    use std::os::unix::io::RawFd;

    // Linux termios constants from kernel headers
    const NCCS: usize = 32;
    const ICANON: u32 = 0o0000002;
    const ECHO: u32 = 0o0000010;
    const VMIN: usize = 6;
    const VTIME: usize = 5;
    const TCSANOW: i32 = 0;
    const TIOCGWINSZ: u64 = 0x5413;

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct Termios {
        pub c_iflag: u32,
        pub c_oflag: u32,
        pub c_cflag: u32,
        pub c_lflag: u32,
        pub c_line: u8,
        pub c_cc: [u8; NCCS],
        pub c_ispeed: u32,
        pub c_ospeed: u32,
    }

    #[repr(C)]
    pub struct Winsize {
        pub ws_row: u16,
        pub ws_col: u16,
        pub ws_xpixel: u16,
        pub ws_ypixel: u16,
    }

    extern "C" {
        fn tcgetattr(fd: RawFd, termios: *mut Termios) -> i32;
        fn tcsetattr(fd: RawFd, action: i32, termios: *const Termios) -> i32;
        fn ioctl(fd: RawFd, request: u64, ...) -> i32;
    }

    pub fn get_termios(fd: RawFd) -> Option<Termios> {
        let mut termios = Termios {
            c_iflag: 0,
            c_oflag: 0,
            c_cflag: 0,
            c_lflag: 0,
            c_line: 0,
            c_cc: [0; NCCS],
            c_ispeed: 0,
            c_ospeed: 0,
        };
        if unsafe { tcgetattr(fd, &mut termios) } == 0 {
            Some(termios)
        } else {
            None
        }
    }

    pub fn set_termios(fd: RawFd, termios: &Termios) -> bool {
        unsafe { tcsetattr(fd, TCSANOW, termios) == 0 }
    }

    /// Create raw mode with timeout for escape sequence detection.
    /// VMIN=0, VTIME=1 means read returns after 100ms timeout or when data available.
    pub fn make_raw_with_timeout(termios: &Termios) -> Termios {
        let mut raw = *termios;
        raw.c_lflag &= !(ICANON | ECHO);
        raw.c_cc[VMIN] = 0;
        raw.c_cc[VTIME] = 1; // 100ms timeout (in 1/10 seconds)
        raw
    }

    pub fn get_winsize(fd: RawFd) -> Option<(u16, u16)> {
        let mut ws = Winsize {
            ws_row: 0,
            ws_col: 0,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        if unsafe { ioctl(fd, TIOCGWINSZ, &mut ws) } == 0 {
            Some((ws.ws_col, ws.ws_row))
        } else {
            None
        }
    }
}

/// RAII guard for terminal raw mode - restores terminal on drop (panic safety)
struct RawModeGuard {
    fd: i32,
    original: term::Termios,
}

impl RawModeGuard {
    fn new(fd: i32) -> Option<Self> {
        let original = term::get_termios(fd)?;
        let raw = term::make_raw_with_timeout(&original);
        if term::set_termios(fd, &raw) {
            Some(Self { fd, original })
        } else {
            None
        }
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        // Restore terminal even on panic
        term::set_termios(self.fd, &self.original);
        // Show cursor and clear screen
        print!("{}{}{}", SHOW_CURSOR, CLEAR, HOME);
        let _ = std::io::Write::flush(&mut std::io::stdout());
    }
}

/// Sanitize a string for safe terminal output.
/// Strips ANSI escape sequences and control characters to prevent terminal injection.
fn sanitize(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Skip ANSI escape sequence: ESC [ ... final_byte
            if chars.peek() == Some(&'[') {
                chars.next(); // consume '['
                // Skip until we hit a letter (final byte of CSI sequence)
                while let Some(&next) = chars.peek() {
                    chars.next();
                    if next.is_ascii_alphabetic() {
                        break;
                    }
                }
            }
            // Also handle ESC followed by other chars (OSC, etc.)
            continue;
        }

        // Allow printable ASCII, newlines, tabs
        if c.is_ascii_graphic() || c == ' ' || c == '\n' || c == '\t' {
            result.push(c);
        } else if c.is_ascii_control() {
            // Replace control chars with a visible marker
            result.push('?');
        } else {
            // Allow non-ASCII Unicode (UTF-8 content)
            result.push(c);
        }
    }

    result
}

// ANSI escape codes
const CLEAR: &str = "\x1b[2J";
const HOME: &str = "\x1b[H";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";
const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const BLUE: &str = "\x1b[34m";
const MAGENTA: &str = "\x1b[35m";
const CYAN: &str = "\x1b[36m";
const HIDE_CURSOR: &str = "\x1b[?25l";
const SHOW_CURSOR: &str = "\x1b[?25h";

/// View state
enum View {
    List,
    Detail(String), // hash
    Heads,
    Search,
}

/// Application state
struct App {
    store: Store,
    view: View,
    blocks: Vec<BlockMeta>,
    heads: Vec<Head>,
    cursor: usize,
    scroll: usize,
    height: usize,
    tag_filter: Option<String>,
    search_query: String,
    message: Option<String>,
}

impl App {
    fn new(store: Store) -> Self {
        let blocks = store.list_blocks(0, 10000, None, None);
        let heads = store.list_heads().unwrap_or_default();
        Self {
            store,
            view: View::List,
            blocks,
            heads,
            cursor: 0,
            scroll: 0,
            height: 20,
            tag_filter: None,
            search_query: String::new(),
            message: None,
        }
    }

    fn refresh_blocks(&mut self) {
        self.blocks = self.store.list_blocks(0, 10000, self.tag_filter.as_deref(), None);
        self.cursor = 0;
        self.scroll = 0;
    }

    fn search(&mut self, query: &str) {
        self.blocks = self.store.search(query, self.tag_filter.as_deref(), 1000);
        self.cursor = 0;
        self.scroll = 0;
        self.search_query = query.to_string();
    }

    fn show_orphans(&mut self) {
        self.blocks = self.store.get_orphans(1000);
        self.cursor = 0;
        self.scroll = 0;
        self.message = Some("Showing orphan blocks (no inbound refs)".to_string());
    }

    fn render(&self) -> String {
        let mut out = String::new();
        out.push_str(CLEAR);
        out.push_str(HOME);

        match &self.view {
            View::List => self.render_list(&mut out),
            View::Detail(hash) => self.render_detail(&mut out, hash),
            View::Heads => self.render_heads(&mut out),
            View::Search => self.render_search(&mut out),
        }

        out
    }

    fn render_list(&self, out: &mut String) {
        // Header
        out.push_str(&format!(
            "{}{}The Fold - Block Explorer{} ",
            BOLD, CYAN, RESET
        ));
        out.push_str(&format!(
            "{}[{} blocks]{}\n",
            DIM,
            self.blocks.len(),
            RESET
        ));

        // Filter info (sanitize user-controlled content)
        if let Some(tag) = &self.tag_filter {
            out.push_str(&format!("{}Tag: {}{}\n", YELLOW, sanitize(tag), RESET));
        }
        if !self.search_query.is_empty() {
            out.push_str(&format!("{}Search: {}{}\n", YELLOW, sanitize(&self.search_query), RESET));
        }
        if let Some(msg) = &self.message {
            out.push_str(&format!("{}{}{}\n", MAGENTA, msg, RESET));
        }

        out.push_str(&format!(
            "{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));

        // Column headers
        out.push_str(&format!(
            "{}  {:16} {:20} {:>8} {:>4} {}{}\n",
            DIM, "HASH", "TAG", "SIZE", "REFS", "PIN", RESET
        ));

        // Blocks
        let visible_height = self.height.saturating_sub(8);

        for (i, block) in self.blocks.iter().enumerate().skip(self.scroll).take(visible_height) {
            let selected = i == self.cursor;
            let prefix = if selected { ">" } else { " " };
            let highlight = if selected { BOLD } else { "" };
            let pin = if block.pinned {
                format!("{}*{}", GREEN, RESET)
            } else {
                " ".to_string()
            };

            // Sanitize tag to prevent terminal injection
            out.push_str(&format!(
                "{}{} {}{:16}{} {:20} {:>8} {:>4} {}\n",
                highlight,
                prefix,
                BLUE,
                &block.hash[..16],
                RESET,
                truncate(&sanitize(&block.tag), 20),
                block.payload_size,
                block.refs_count,
                pin,
            ));
        }

        // Footer
        out.push_str(&format!(
            "\n{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));
        out.push_str(&format!(
            "{}j/k{} move  {}Enter{} view  {}/{} search  {}t{} tag  {}o{} orphans  {}h{} heads  {}q{} quit\n",
            BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET
        ));

        // Scroll indicator
        if self.blocks.len() > visible_height {
            let pos = if self.blocks.is_empty() {
                0
            } else {
                (self.cursor * 100) / self.blocks.len()
            };
            out.push_str(&format!("{}[{}/{}] {}%{}\n", DIM, self.cursor + 1, self.blocks.len(), pos, RESET));
        }
    }

    fn render_detail(&self, out: &mut String, hash: &str) {
        out.push_str(&format!(
            "{}{}Block Detail{}\n",
            BOLD, CYAN, RESET
        ));
        out.push_str(&format!(
            "{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));

        // Try to load block
        match self.store.load_block(hash) {
            Ok(block) => {
                let meta = self.store.get_meta(hash);
                let pinned = meta.as_ref().map(|m| m.pinned).unwrap_or(false);

                out.push_str(&format!("{}Hash:{} {}{}{}\n", BOLD, RESET, BLUE, hash, RESET));
                // Sanitize tag to prevent terminal injection
                out.push_str(&format!("{}Tag:{} {}\n", BOLD, RESET, sanitize(&block.tag)));
                out.push_str(&format!("{}Size:{} {} bytes\n", BOLD, RESET, block.payload.len()));
                out.push_str(&format!(
                    "{}Pinned:{} {}\n",
                    BOLD,
                    RESET,
                    if pinned {
                        format!("{}Yes{}", GREEN, RESET)
                    } else {
                        "No".to_string()
                    }
                ));

                // Payload preview (sanitize to prevent terminal injection)
                out.push_str(&format!("\n{}Payload:{}\n", BOLD, RESET));
                let preview = sanitize(&block.payload_preview(500));
                for line in preview.lines().take(10) {
                    out.push_str(&format!("  {}{}{}\n", DIM, line, RESET));
                }
                if preview.lines().count() > 10 {
                    out.push_str(&format!("  {}...{}\n", DIM, RESET));
                }

                // Refs (sanitize tags)
                if !block.refs.is_empty() {
                    out.push_str(&format!("\n{}References ({}):{}\n", BOLD, block.refs.len(), RESET));
                    for (i, r) in block.refs.iter().enumerate().take(10) {
                        let ref_hash = r.to_hex();
                        let ref_meta = self.store.get_meta(&ref_hash);
                        let ref_tag = ref_meta.map(|m| sanitize(&m.tag)).unwrap_or_else(|| "?".to_string());
                        let marker = if i == self.cursor { ">" } else { " " };
                        out.push_str(&format!(
                            "  {}{} {}{}{} {}\n",
                            marker, BLUE, &ref_hash[..24], RESET, DIM, ref_tag
                        ));
                    }
                }
            }
            Err(e) => {
                out.push_str(&format!("{}Error:{} {}\n", RED, RESET, e));
            }
        }

        out.push_str(&format!(
            "\n{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));
        out.push_str(&format!(
            "{}j/k{} select ref  {}Enter{} follow  {}b/Esc{} back  {}q{} quit\n",
            BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET
        ));
    }

    fn render_heads(&self, out: &mut String) {
        out.push_str(&format!(
            "{}{}Heads (Named References){}\n",
            BOLD, CYAN, RESET
        ));
        out.push_str(&format!(
            "{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));

        let visible_height = self.height.saturating_sub(6);
        for (i, head) in self.heads.iter().enumerate().skip(self.scroll).take(visible_height) {
            let selected = i == self.cursor;
            let prefix = if selected { ">" } else { " " };
            let highlight = if selected { BOLD } else { "" };

            // Sanitize head name to prevent terminal injection
            out.push_str(&format!(
                "{}{} {:24} {}{}{}\n",
                highlight,
                prefix,
                truncate(&sanitize(&head.name), 24),
                BLUE,
                &head.hash[..32.min(head.hash.len())],
                RESET
            ));
        }

        out.push_str(&format!(
            "\n{}─────────────────────────────────────────────────────────────────{}\n",
            DIM, RESET
        ));
        out.push_str(&format!(
            "{}j/k{} move  {}Enter{} view  {}b/Esc{} back  {}q{} quit\n",
            BOLD, RESET, BOLD, RESET, BOLD, RESET, BOLD, RESET
        ));
    }

    fn render_search(&self, out: &mut String) {
        out.push_str(&format!("{}Search:{} {}_\n", BOLD, RESET, self.search_query));
    }

    fn handle_key(&mut self, key: u8) -> bool {
        match &self.view {
            View::List => self.handle_list_key(key),
            View::Detail(hash) => {
                let hash = hash.clone();
                self.handle_detail_key(key, &hash)
            }
            View::Heads => self.handle_heads_key(key),
            View::Search => self.handle_search_key(key),
        }
    }

    fn handle_list_key(&mut self, key: u8) -> bool {
        match key {
            b'q' => return false,
            b'j' | 66 => {
                // Down
                if self.cursor < self.blocks.len().saturating_sub(1) {
                    self.cursor += 1;
                    if self.cursor >= self.scroll + self.height.saturating_sub(8) {
                        self.scroll += 1;
                    }
                }
            }
            b'k' | 65 => {
                // Up
                if self.cursor > 0 {
                    self.cursor -= 1;
                    if self.cursor < self.scroll {
                        self.scroll = self.cursor;
                    }
                }
            }
            13 | 10 => {
                // Enter
                if let Some(block) = self.blocks.get(self.cursor) {
                    self.view = View::Detail(block.hash.clone());
                    self.cursor = 0;
                }
            }
            b'/' => {
                self.view = View::Search;
                self.search_query.clear();
            }
            b't' => {
                // Cycle through tags
                let tags = self.store.tag_counts();
                let mut tag_list: Vec<_> = tags.keys().collect();
                tag_list.sort();

                if let Some(current) = &self.tag_filter {
                    let idx = tag_list.iter().position(|t| *t == current);
                    if let Some(i) = idx {
                        if i + 1 < tag_list.len() {
                            self.tag_filter = Some(tag_list[i + 1].clone());
                        } else {
                            self.tag_filter = None;
                        }
                    } else {
                        self.tag_filter = None;
                    }
                } else if !tag_list.is_empty() {
                    self.tag_filter = Some(tag_list[0].clone());
                }
                self.message = None;
                self.search_query.clear();
                self.refresh_blocks();
            }
            b'o' => {
                self.tag_filter = None;
                self.show_orphans();
            }
            b'p' => {
                let popular = self.store.get_popular(100);
                self.blocks = popular.into_iter().map(|(m, _)| m).collect();
                self.cursor = 0;
                self.scroll = 0;
                self.message = Some("Showing popular blocks (most inbound refs)".to_string());
            }
            b'h' => {
                self.view = View::Heads;
                self.cursor = 0;
                self.scroll = 0;
            }
            b'r' => {
                self.tag_filter = None;
                self.search_query.clear();
                self.message = None;
                self.refresh_blocks();
            }
            _ => {}
        }
        true
    }

    fn handle_detail_key(&mut self, key: u8, hash: &str) -> bool {
        match key {
            b'q' => return false,
            b'b' | 27 => {
                // Back
                self.view = View::List;
                self.cursor = 0;
            }
            b'j' | 66 => {
                // Down - move through refs
                if let Ok(block) = self.store.load_block(hash) {
                    if self.cursor < block.refs.len().saturating_sub(1) {
                        self.cursor += 1;
                    }
                }
            }
            b'k' | 65 => {
                // Up
                if self.cursor > 0 {
                    self.cursor -= 1;
                }
            }
            13 | 10 => {
                // Enter - follow ref
                if let Ok(block) = self.store.load_block(hash) {
                    if let Some(r) = block.refs.get(self.cursor) {
                        let ref_hash = r.to_hex();
                        self.view = View::Detail(ref_hash);
                        self.cursor = 0;
                    }
                }
            }
            _ => {}
        }
        true
    }

    fn handle_heads_key(&mut self, key: u8) -> bool {
        match key {
            b'q' => return false,
            b'b' | 27 => {
                self.view = View::List;
                self.cursor = 0;
            }
            b'j' | 66 => {
                if self.cursor < self.heads.len().saturating_sub(1) {
                    self.cursor += 1;
                }
            }
            b'k' | 65 => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                }
            }
            13 | 10 => {
                if let Some(head) = self.heads.get(self.cursor) {
                    self.view = View::Detail(head.hash.clone());
                    self.cursor = 0;
                }
            }
            _ => {}
        }
        true
    }

    fn handle_search_key(&mut self, key: u8) -> bool {
        match key {
            27 => {
                // Escape
                self.view = View::List;
            }
            13 | 10 => {
                // Enter - execute search
                let query = self.search_query.clone();
                self.search(&query);
                self.view = View::List;
            }
            127 | 8 => {
                // Backspace
                self.search_query.pop();
            }
            32..=126 => {
                // Printable char
                self.search_query.push(key as char);
            }
            _ => {}
        }
        true
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max - 3])
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // Check for --check mode (non-interactive store verification)
    let check_mode = args.iter().any(|a| a == "--check");

    let store_path = args
        .iter()
        .skip(1)
        .find(|a| !a.starts_with('-'))
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(".store"));

    // Load store
    eprintln!("Loading store from: {:?}", store_path);
    let store = match Store::open(&store_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to open store: {}", e);
            process::exit(1);
        }
    };

    let block_count = store.block_count();
    eprintln!("Indexed {} blocks", block_count);

    // Non-interactive check mode - just report stats and exit
    if check_mode {
        let tags = store.tag_counts();
        println!("Store verification:");
        println!("  Total blocks: {}", block_count);
        println!("  Unique tags: {}", tags.len());
        for (tag, count) in tags.iter().take(10) {
            println!("    {}: {}", tag, count);
        }
        let orphans = store.get_orphans(10);
        println!("  Orphan blocks (first 10): {}", orphans.len());
        let heads = store.list_heads().unwrap_or_default();
        println!("  Named heads: {}", heads.len());
        process::exit(0);
    }

    // Set up terminal raw mode with RAII guard (panic safety)
    let fd = io::stdin().as_raw_fd();
    let _guard = match RawModeGuard::new(fd) {
        Some(g) => g,
        None => {
            eprintln!("Failed to set raw terminal mode");
            process::exit(1);
        }
    };

    print!("{}", HIDE_CURSOR);
    io::stdout().flush().ok();

    let mut app = App::new(store);
    let stdout_fd = io::stdout().as_raw_fd();

    // Main loop
    let mut stdin = io::stdin();
    loop {
        // Dynamic window resize - check each iteration
        if let Some((_, h)) = term::get_winsize(stdout_fd) {
            app.height = h as usize;
        }

        // Render
        print!("{}", app.render());
        io::stdout().flush().ok();

        // Read input (with VTIME=1 timeout, read returns 0 bytes on timeout)
        let mut buf = [0u8; 3];
        match stdin.read(&mut buf[..1]) {
            Ok(0) => continue, // Timeout, no input - loop again
            Ok(_) => {}
            Err(_) => continue,
        }

        let key = buf[0];

        // Handle escape sequences (arrow keys send \x1b[A etc)
        if key == 27 {
            // Try to read more bytes with timeout
            // If timeout (0 bytes), it was just ESC key
            let n = stdin.read(&mut buf[1..3]).unwrap_or(0);
            if n >= 2 && buf[1] == b'[' {
                // Arrow key sequence: pass the direction byte
                if !app.handle_key(buf[2]) {
                    break;
                }
            } else {
                // Plain ESC key (or incomplete sequence)
                if !app.handle_key(27) {
                    break;
                }
            }
        } else if !app.handle_key(key) {
            break;
        }
    }

    // Guard's Drop will restore terminal
}
