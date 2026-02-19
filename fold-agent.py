#!/usr/bin/env python3
"""
fold — JSON-based REPL client for The Fold.

Usage:
  ./fold "(+ 1 2)"                      # Evaluate an expression
  ./fold -s dev "(define x 10)"         # Named session with -s flag
  ./fold --status                       # Check if daemon is running
  ./fold --sessions                     # List active sessions
  ./fold -c file.ss                     # Check file for paren errors

Features:
  - Auto-starts daemon if not running (disable with --no-auto-start)
  - Colorized error output (disable with NO_COLOR=1)

Session Persistence:
  Sessions can be specified in several ways (in order of priority):
  1. --session/-s flag: ./fold -s dev "code"
  2. FOLD_SESSION env var: export FOLD_SESSION=dev
  3. .fold-session file: Contains session name (created by --persist)
  4. Default: "agent-default" (stable session for agent workflows)

  Use --persist to save a custom session name to .fold-session.
  For ephemeral sessions: --session "agent-$(uuidgen | head -c 8)"

Output (JSON with --verbose):
  {
    "status": "success",   # or "error", "timeout"
    "result": "3",         # The return value
    "session": "my-session",
    "error": null
  }

Exit codes:
  0 - Success
  1 - Error (daemon, execution, etc.)
  2 - Timeout

Note: Code is passed to the daemon as-is. Use explicit parentheses.
"""

import os
import sys
import json
import time
import uuid
import argparse
import subprocess
import shutil
import socket as sock_mod
import struct

# Configuration
REPL_DIR = ".fold-repl"
REQUESTS_DIR = os.path.join(REPL_DIR, "requests")
RESPONSES_DIR = os.path.join(REPL_DIR, "responses")
READY_FILE = os.path.join(REPL_DIR, "ready")
SESSION_FILE = ".fold-session"  # Persisted session file
DAEMON_SCRIPT = "./daemon.sh"
DEFAULT_TIMEOUT = 120  # 2 minutes - allows for heavy operations like BBS indexing
DAEMON_START_TIMEOUT = 30  # Max seconds to wait for daemon to start

# Terminal colors (disabled if not a tty or NO_COLOR is set)
def supports_color():
    return sys.stderr.isatty() and os.environ.get("NO_COLOR") is None

COLORS = {
    "red": "\033[91m",
    "green": "\033[92m",
    "yellow": "\033[93m",
    "dim": "\033[2m",
    "reset": "\033[0m"
} if supports_color() else {k: "" for k in ["red", "green", "yellow", "dim", "reset"]}

def get_session_from_env():
    """Get session from FOLD_SESSION environment variable."""
    return os.environ.get("FOLD_SESSION")

def get_session_from_file():
    """Get session from .fold-session file if it exists."""
    if os.path.exists(SESSION_FILE):
        try:
            with open(SESSION_FILE, 'r') as f:
                session = f.read().strip()
                if session:
                    return session
        except IOError:
            pass
    return None

def save_session_to_file(session_id):
    """Save session to .fold-session file."""
    try:
        with open(SESSION_FILE, 'w') as f:
            f.write(session_id)
        return True
    except IOError:
        return False

def resolve_session(explicit_session):
    """
    Resolve session ID from multiple sources.
    Priority: explicit > env var > file > auto-generate
    """
    if explicit_session:
        return explicit_session
    env_session = get_session_from_env()
    if env_session:
        return env_session
    file_session = get_session_from_file()
    if file_session:
        return file_session
    return generate_session_id()

def list_sessions():
    """List active sessions by finding running worker processes."""
    import subprocess
    import re

    sessions = []
    now = time.time()

    # Find worker processes via ps — works with both file-based and socket daemons
    try:
        result = subprocess.run(
            ["ps", "aux"],
            capture_output=True, text=True, timeout=5
        )
        for line in result.stdout.splitlines():
            # Match the scheme process, not the sh -c wrapper
            if '/bin/sh -c' in line:
                continue
            # Match: scheme --script .../repl-worker-socket.ss <session-id>
            # or:    scheme --script .../repl-worker.ss <session-id>
            match = re.search(r'repl-worker(?:-socket)?\.ss\s+(\S+)', line)
            if match:
                session_id = match.group(1)
                # Extract PID (second field in ps aux output)
                parts = line.split()
                try:
                    pid = int(parts[1])
                except (IndexError, ValueError):
                    continue

                # Extract process start time for idle estimate
                # ps aux TIME field (index 9) shows cumulative CPU, not helpful
                # Use /proc/<pid>/stat start time if available
                idle_seconds = None
                try:
                    stat_file = f"/proc/{pid}/stat"
                    if os.path.exists(stat_file):
                        with open(stat_file, 'r') as f:
                            # Field 22 is starttime in clock ticks
                            fields = f.read().split()
                            if len(fields) > 21:
                                clk_tck = os.sysconf('SC_CLK_TCK')
                                boot_time = 0
                                with open('/proc/stat', 'r') as bf:
                                    for bline in bf:
                                        if bline.startswith('btime'):
                                            boot_time = int(bline.split()[1])
                                            break
                                start_time = boot_time + int(fields[21]) / clk_tck
                                idle_seconds = now - start_time
                except (IOError, ValueError, OSError):
                    pass

                sessions.append({
                    "session_id": session_id,
                    "name": session_id,
                    "pid": pid,
                    "idle_seconds": idle_seconds
                })
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    return sessions

def format_idle_time(seconds):
    """Format idle time as human-readable string."""
    if seconds is None:
        return "unknown"
    if seconds < 60:
        return f"{int(seconds)}s"
    if seconds < 3600:
        return f"{int(seconds / 60)}m"
    if seconds < 86400:
        return f"{int(seconds / 3600)}h"
    return f"{int(seconds / 86400)}d"

def ensure_dirs():
    os.makedirs(REQUESTS_DIR, exist_ok=True)
    os.makedirs(RESPONSES_DIR, exist_ok=True)


def is_daemon_running():
    """Check if daemon is actually running (not just zombie state)."""
    if not os.path.exists(READY_FILE):
        return False

    # Verify the PID is actually alive
    pid_file = os.path.join(REPL_DIR, "daemon.pid")
    if not os.path.exists(pid_file):
        # Ready file without PID file = zombie state
        return False

    try:
        with open(pid_file, 'r') as f:
            raw = f.read().strip()
        if not raw or not raw.isdigit():
            return False  # Corrupt PID file
        pid = int(raw)
        if pid <= 0:
            return False
        os.kill(pid, 0)  # Check if process exists
        return True
    except (IOError, ValueError, OSError):
        # PID file unreadable or process not running - zombie state
        return False

def cleanup_zombie_state():
    """Clean up zombie daemon state (ready file without running process)."""
    pid_file = os.path.join(REPL_DIR, "daemon.pid")
    # Use try/except to handle concurrent cleanup safely
    for path in [READY_FILE, pid_file]:
        try:
            os.remove(path)
        except FileNotFoundError:
            pass  # Already removed by another process

def start_daemon(verbose=False):
    """Attempt to start the daemon. Returns True if successful."""
    if not os.path.exists(DAEMON_SCRIPT):
        return False, "daemon.sh not found"

    if verbose:
        print(f"{COLORS['dim']}Starting REPL daemon...{COLORS['reset']}", file=sys.stderr)

    try:
        # Start daemon in background
        result = subprocess.run(
            [DAEMON_SCRIPT, "start"],
            capture_output=True,
            text=True,
            timeout=10
        )

        # Wait for ready file to appear
        start_time = time.time()
        while time.time() - start_time < DAEMON_START_TIMEOUT:
            if is_daemon_running():
                if verbose:
                    print(f"{COLORS['green']}Daemon started.{COLORS['reset']}", file=sys.stderr)
                return True, None
            time.sleep(0.2)

        return False, "Daemon started but ready file not created"
    except subprocess.TimeoutExpired:
        return False, "daemon.sh start timed out"
    except Exception as e:
        return False, str(e)

def ensure_daemon_running(verbose=False, auto_start=True):
    """Ensure daemon is running, optionally auto-starting it."""
    if is_daemon_running():
        return True, None

    # Check for zombie state: ready file exists but process not running
    if os.path.exists(READY_FILE):
        if verbose:
            print(f"{COLORS['yellow']}Daemon in zombie state, cleaning up...{COLORS['reset']}", file=sys.stderr)
        cleanup_zombie_state()

    if not auto_start:
        return False, "REPL daemon is not running. Run './daemon.sh start' first."

    return start_daemon(verbose)

def generate_session_id():
    """Generate a stable default session ID.

    Uses 'agent-default' instead of random UUID so agents maintain state
    across invocations without explicitly specifying --session.

    For true ephemeral sessions, use: ./fold-agent.py --session "agent-$(uuidgen | head -c 8)"
    """
    return "agent-default"

def get_socket_path():
    """Check if socket daemon is running and return socket path, or None."""
    ready_file = os.path.join(REPL_DIR, "ready")
    if not os.path.exists(ready_file):
        return None
    try:
        with open(ready_file, 'r') as f:
            content = f.read().strip()
        if content.startswith("socket:"):
            sock_path = content[len("socket:"):]
            if os.path.exists(sock_path):
                return sock_path
    except IOError:
        pass
    return None

def recv_exact(s, n):
    """Receive exactly n bytes from socket."""
    buf = b''
    while len(buf) < n:
        chunk = s.recv(n - len(buf))
        if not chunk:
            raise ConnectionError("Socket closed during read")
        buf += chunk
    return buf

def run_request_socket(session_id, code, timeout, sock_path):
    """Send request via Unix domain socket and receive response."""
    s = sock_mod.socket(sock_mod.AF_UNIX, sock_mod.SOCK_STREAM)
    s.settimeout(timeout)
    try:
        s.connect(sock_path)

        # Build s-expression request
        req_id = uuid.uuid4().hex[:8]
        # Escape the expression for s-expression embedding
        escaped_code = code.replace('\\', '\\\\').replace('"', '\\"')
        msg = f'((type . request) (id . "{req_id}") (session . "{session_id}") (expr . "{escaped_code}"))'
        data = msg.encode('utf-8')

        # Send length-prefixed frame
        s.sendall(struct.pack('>I', len(data)) + data)

        # Read response frame
        length_bytes = recv_exact(s, 4)
        length = struct.unpack('>I', length_bytes)[0]
        if length > 16 * 1024 * 1024:
            return {
                "status": "error",
                "result": None,
                "session": session_id,
                "error": f"Response too large: {length} bytes"
            }
        payload = recv_exact(s, length)
        resp_str = payload.decode('utf-8')

        # Parse s-expression response (simple extraction)
        return parse_sexp_response(resp_str, session_id)
    except sock_mod.timeout:
        return {
            "status": "timeout",
            "result": None,
            "session": session_id,
            "error": f"Request timed out after {timeout}s"
        }
    except Exception as e:
        return {
            "status": "error",
            "result": None,
            "session": session_id,
            "error": str(e)
        }
    finally:
        s.close()

def parse_sexp_response(resp_str, session_id):
    """Parse an s-expression response alist into a result dict."""
    # Extract type, value, message fields from the alist
    import re
    type_match = re.search(r'\(type\s+\.\s+(\w+)\)', resp_str)
    msg_type = type_match.group(1) if type_match else "unknown"

    if msg_type == "result":
        # Extract value — it's after (value . "...")
        value_match = re.search(r'\(value\s+\.\s+"((?:[^"\\]|\\.)*)"\)', resp_str)
        value = value_match.group(1).replace('\\"', '"').replace('\\\\', '\\') if value_match else ""
        return {
            "status": "success",
            "result": value,
            "session": session_id,
            "error": None
        }
    elif msg_type == "error":
        err_match = re.search(r'\(message\s+\.\s+"((?:[^"\\]|\\.)*)"\)', resp_str)
        error_msg = err_match.group(1).replace('\\"', '"').replace('\\\\', '\\') if err_match else "unknown error"
        return {
            "status": "error",
            "result": None,
            "session": session_id,
            "error": error_msg
        }
    else:
        return {
            "status": "error",
            "result": None,
            "session": session_id,
            "error": f"Unexpected response type: {msg_type}"
        }

def run_request(session_id, code, timeout):
    # Try socket transport first
    sock_path = get_socket_path()
    if sock_path:
        return run_request_socket(session_id, code, timeout, sock_path)

    # Fall back to file-based IPC
    request_file = os.path.join(REQUESTS_DIR, f"{session_id}.ss")
    response_file = os.path.join(RESPONSES_DIR, f"{session_id}.txt")
    error_file = os.path.join(RESPONSES_DIR, f"{session_id}.error.txt")

    # Clean up previous responses for this session
    if os.path.exists(response_file):
        os.remove(response_file)
    if os.path.exists(error_file):
        os.remove(error_file)

    # Write request
    with open(request_file, 'w') as f:
        f.write(code)

    # Poll for response
    start_time = time.time()
    while time.time() - start_time < timeout:
        if os.path.exists(response_file):
            # Success (or at least execution finished)
            with open(response_file, 'r') as f:
                output_content = f.read()
            
            # Check for separate error file (runtime errors)
            error_content = None
            if os.path.exists(error_file):
                with open(error_file, 'r') as f:
                    error_content = f.read()
            
            # The worker format:
            # If successful, output_content contains stdout + \n + result (sometimes)
            # The worker logic `scheme-eval-and-capture` returns:
            # output + result.
            
            return {
                "status": "error" if error_content else "success",
                "result": output_content.strip(), # It's hard to separate stdout/result without parsing, returning full blob
                "session": session_id,
                "error": error_content
            }
            
        if os.path.exists(error_file):
            # Error only (e.g. read error)
            with open(error_file, 'r') as f:
                error_content = f.read()
            return {
                "status": "error",
                "result": None,
                "session": session_id,
                "error": error_content
            }
            
        time.sleep(0.1)

    return {
        "status": "timeout",
        "result": None,
        "session": session_id,
        "error": f"Request timed out after {timeout}s"
    }

def check_syntax(filepath):
    """Check file for parenthesis balance errors.

    Runs paren-locate from paren-check.ss directly via scheme.
    Returns 0 if balanced, 1 if errors found.
    """
    if not os.path.exists(filepath):
        print(f"{COLORS['red']}Error:{COLORS['reset']} File not found: {filepath}", file=sys.stderr)
        return 1

    # Handle stdin paths: buffer to temp file since subprocess.run(input=...)
    # redirects the child's stdin, making /dev/stdin read scheme_code instead
    # of the original piped input.
    if filepath in ("/dev/stdin", "-"):
        import tempfile
        content = sys.stdin.read()
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ss', delete=False) as f:
            f.write(content)
            temp_path = f.name
        try:
            return check_syntax(temp_path)
        finally:
            os.unlink(temp_path)

    # Run scheme directly to check parens (no daemon needed)
    scheme_code = f'''
(load "boundary/tools/paren-check.ss")
(let ([errors (paren-errors "{filepath}")])
  (if (null? errors)
      (begin
        (printf "~a~a: ✓ All parentheses balanced~a~n"
                "{COLORS['green']}" "{filepath}" "{COLORS['reset']}")
        (exit 0))
      (begin
        (for-each
          (lambda (err)
            (printf "~a:~a:~a: ~a~aerror~a: ~a~n"
                    "{filepath}"
                    (caddr err)   ; line
                    (+ (cadddr err) 1)  ; col (1-indexed)
                    "{COLORS['red']}"
                    ""
                    "{COLORS['reset']}"
                    (cadr err)))  ; message
          errors)
        (exit 1))))
'''

    try:
        result = subprocess.run(
            ["scheme", "-q"],
            input=scheme_code,
            capture_output=True,
            text=True,
            timeout=30
        )
        # Print output
        if result.stdout:
            print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        return result.returncode
    except subprocess.TimeoutExpired:
        print(f"{COLORS['red']}Error:{COLORS['reset']} Syntax check timed out", file=sys.stderr)
        return 1
    except FileNotFoundError:
        print(f"{COLORS['red']}Error:{COLORS['reset']} 'scheme' not found in PATH", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"{COLORS['red']}Error:{COLORS['reset']} {e}", file=sys.stderr)
        return 1


def main():
    parser = argparse.ArgumentParser(
        description="Fold REPL Agent Client",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  ./fold "(+ 1 2)"              Evaluate (+ 1 2)
  ./fold -s dev "(define x 10)" Define x in 'dev' session
  ./fold --status               Check if daemon is running
  ./fold --sessions             List active sessions
  ./fold --no-auto-start "(x)"  Don't auto-start daemon
  ./fold -c file.ss             Check file for paren balance errors
"""
    )
    parser.add_argument("code", nargs="*", help="Code to execute (multiple args joined with spaces)")
    parser.add_argument("--session", "-s", help="Session ID (overrides env/file)")
    parser.add_argument("--persist", "-p", action="store_true",
                        help="Save session to .fold-session for future calls")
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT, help="Timeout in seconds")
    parser.add_argument("--json", action="store_true", help="Read JSON input from stdin")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Verbose mode: output full JSON response")
    parser.add_argument("--show-session", action="store_true",
                        help="Show current session and exit")
    parser.add_argument("--status", action="store_true",
                        help="Check daemon status and exit")
    parser.add_argument("--sessions", action="store_true",
                        help="List active sessions and exit")
    parser.add_argument("--no-auto-start", action="store_true",
                        help="Don't auto-start daemon if not running")
    parser.add_argument("--check-syntax", "-c", metavar="FILE",
                        help="Check file for paren balance errors (no execution)")

    args = parser.parse_args()

    # Handle --check-syntax early (doesn't need daemon)
    if args.check_syntax:
        sys.exit(check_syntax(args.check_syntax))

    # Handle --status early
    if args.status:
        if is_daemon_running():
            print(f"{COLORS['green']}Daemon is running{COLORS['reset']}")
            sys.exit(0)
        else:
            print(f"{COLORS['yellow']}Daemon is not running{COLORS['reset']}")
            sys.exit(1)

    # Handle --sessions early
    if args.sessions:
        sessions = list_sessions()
        if not sessions:
            print(f"{COLORS['yellow']}No active sessions{COLORS['reset']}")
            sys.exit(0)
        print(f"{COLORS['green']}Active sessions:{COLORS['reset']}")
        for s in sessions:
            name = s["name"] or s["session_id"][:12]
            idle = format_idle_time(s["idle_seconds"])
            print(f"  {name:<20} (pid {s['pid']}, idle {idle})")
        sys.exit(0)

    explicit_session = args.session
    code = " ".join(args.code) if args.code else None
    timeout = args.timeout
    code_from_args = code is not None  # Track whether code came from CLI args

    if args.json:
        try:
            data = json.load(sys.stdin)
            code = data.get("code", code)
            explicit_session = data.get("session", explicit_session)
            timeout = data.get("timeout", timeout)
        except json.JSONDecodeError as e:
            print(json.dumps({"status": "error", "error": f"Invalid JSON input: {e}"}))
            return

    # Resolve session from multiple sources
    session_id = resolve_session(explicit_session)

    # Handle --show-session
    if args.show_session:
        source = "explicit" if explicit_session else (
            "FOLD_SESSION env" if get_session_from_env() else (
            ".fold-session file" if get_session_from_file() else "auto-generated"))
        print(json.dumps({"session": session_id, "source": source}))
        return

    if not code and not code_from_args:
        # Only try stdin when no CLI args were given (supports piping)
        if not sys.stdin.isatty():
            code = sys.stdin.read()

    if not code or not code.strip():
        if args.verbose or args.json:
            print(json.dumps({"status": "error", "error": "No code provided"}))
        else:
            print(f"{COLORS['red']}Error:{COLORS['reset']} No code provided", file=sys.stderr)
            sys.exit(1)
        return

    ensure_dirs()

    # Handle --persist: save session before running
    if args.persist:
        save_session_to_file(session_id)

    # Ensure daemon is running (auto-start unless --no-auto-start)
    daemon_ok, daemon_error = ensure_daemon_running(
        verbose=not args.json,  # Show startup messages unless JSON mode
        auto_start=not args.no_auto_start
    )

    if not daemon_ok:
        if args.verbose or args.json:
            print(json.dumps({
                "status": "error",
                "error": daemon_error
            }))
        else:
            print(f"{COLORS['red']}Error:{COLORS['reset']} {daemon_error}", file=sys.stderr)
            sys.exit(1)
        return

    response = run_request(session_id, code, timeout)

    if args.verbose:
        # Verbose mode: full JSON response
        print(json.dumps(response))
    else:
        # Default: just output result, errors to stderr
        if response["status"] == "success":
            print(response["result"])
        elif response["status"] == "timeout":
            print(f"{COLORS['yellow']}Timeout:{COLORS['reset']} {response.get('error', 'Request timed out')}", file=sys.stderr)
            sys.exit(2)
        else:
            print(f"{COLORS['red']}Error:{COLORS['reset']} {response.get('error', 'Unknown error')}", file=sys.stderr)
            sys.exit(1)

if __name__ == "__main__":
    main()
