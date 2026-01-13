#!/usr/bin/env python3
"""
fold-agent.py — JSON-based REPL client for LLM Agents.

Usage:
  ./fold-agent.py "+ 1 2"                              # Implicit parens: (+ 1 2)
  ./fold-agent.py "(+ 1 2)"                            # Explicit parens work too
  ./fold-agent.py --session my-session "define x 10"  # Becomes (define x 10)
  echo '{"code": "+ 1 2", "session": "my-session"}' | ./fold-agent.py --json

Output (JSON):
  {
    "status": "success",   # or "error", "timeout"
    "result": "3",         # The return value content-address or representation
    "output": "",          # Stdout captured during execution
    "session": "my-session",
    "error": null
  }

Note: If code doesn't start with '(', it's automatically wrapped in parens.
      Single tokens stay unwrapped (e.g., "x" stays "x", not "(x)").
"""

import os
import sys
import json
import time
import uuid
import argparse
import subprocess

# Configuration
REPL_DIR = ".fold-repl"
REQUESTS_DIR = os.path.join(REPL_DIR, "requests")
RESPONSES_DIR = os.path.join(REPL_DIR, "responses")
READY_FILE = os.path.join(REPL_DIR, "ready")
DEFAULT_TIMEOUT = 30

def ensure_dirs():
    os.makedirs(REQUESTS_DIR, exist_ok=True)
    os.makedirs(RESPONSES_DIR, exist_ok=True)

def apply_implicit_parens(code):
    """Wrap code in parens if it doesn't start with '(' and has multiple tokens."""
    code = code.strip()
    if not code:
        return code
    # Already parenthesized
    if code.startswith('('):
        return code
    # Single token (no spaces outside of strings) - leave as-is
    # Simple heuristic: if no whitespace, it's a single token
    if ' ' not in code and '\t' not in code and '\n' not in code:
        return code
    # Multiple tokens - wrap in parens
    return f"({code})"

def is_daemon_running():
    return os.path.exists(READY_FILE)

def generate_session_id():
    return f"agent-{uuid.uuid4().hex[:8]}"

def run_request(session_id, code, timeout):
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

def main():
    parser = argparse.ArgumentParser(description="Fold REPL Agent Client")
    parser.add_argument("code", nargs="?", help="Code to execute")
    parser.add_argument("--session", help="Session ID")
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT, help="Timeout in seconds")
    parser.add_argument("--json", action="store_true", help="Read JSON input from stdin")
    
    args = parser.parse_args()
    
    session_id = args.session
    code = args.code
    timeout = args.timeout

    if args.json:
        try:
            data = json.load(sys.stdin)
            code = data.get("code", code)
            session_id = data.get("session", session_id)
            timeout = data.get("timeout", timeout)
        except json.JSONDecodeError as e:
            print(json.dumps({"status": "error", "error": f"Invalid JSON input: {e}"}))
            return

    if not code:
        # Try reading code from stdin if not in json mode and not arg
        if not sys.stdin.isatty():
            code = sys.stdin.read()

    if not code:
        print(json.dumps({"status": "error", "error": "No code provided"}))
        return

    # Implicit parenthesization: wrap code in parens if needed
    code = apply_implicit_parens(code)

    if not session_id:
        session_id = generate_session_id()

    ensure_dirs()

    if not is_daemon_running():
        # Fallback handling could go here, but for now just report error
        # Agents should know to start the daemon or use fold.sh's fallback logic if needed.
        # But to be helpful, let's try to run via fold.sh's fallback logic?
        # No, let's stick to the daemon protocol for reliability.
        print(json.dumps({
            "status": "error", 
            "error": "REPL daemon is not running. Run './start-daemon.ss' first."
        }))
        return

    response = run_request(session_id, code, timeout)
    print(json.dumps(response))

if __name__ == "__main__":
    main()
