#!/usr/bin/env python3
"""
Test the fold-lsp server by sending LSP messages.

Usage: python3 boundary/lsp/test-lsp.py
"""

import subprocess
import json
import sys
import os

# Change to project root
project_root = os.environ.get("FOLD_ROOT", os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
os.chdir(project_root)

def make_request(id, method, params=None):
    """Create an LSP request."""
    msg = {
        "jsonrpc": "2.0",
        "id": id,
        "method": method
    }
    if params is not None:
        msg["params"] = params
    return msg

def make_notification(method, params=None):
    """Create an LSP notification."""
    msg = {
        "jsonrpc": "2.0",
        "method": method
    }
    if params is not None:
        msg["params"] = params
    return msg

def encode_message(msg):
    """Encode a message with Content-Length header."""
    body = json.dumps(msg)
    header = f"Content-Length: {len(body)}\r\n\r\n"
    return (header + body).encode('utf-8')

def decode_response(data):
    """Decode an LSP response."""
    # Find the header/body boundary
    text = data.decode('utf-8')
    parts = text.split('\r\n\r\n', 1)
    if len(parts) < 2:
        return None
    return json.loads(parts[1])

def test_server():
    """Run basic LSP tests."""
    print("Starting fold-lsp server...")

    # Start the server
    proc = subprocess.Popen(
        ["scheme", "--script", "boundary/lsp/start-lsp.ss"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )

    try:
        # Test 1: Initialize
        print("\n--- Test 1: Initialize ---")
        init_request = make_request(1, "initialize", {
            "processId": os.getpid(),
            "rootUri": f"file://{project_root}",
            "capabilities": {}
        })
        proc.stdin.write(encode_message(init_request))
        proc.stdin.flush()

        # Read response
        response_data = b""
        while True:
            chunk = proc.stdout.read(1)
            if not chunk:
                break
            response_data += chunk
            if b"\r\n\r\n" in response_data:
                # Read the body
                header = response_data.split(b"\r\n\r\n")[0].decode('utf-8')
                for line in header.split("\r\n"):
                    if line.startswith("Content-Length:"):
                        length = int(line.split(":")[1].strip())
                        body_start = response_data.find(b"\r\n\r\n") + 4
                        remaining = length - (len(response_data) - body_start)
                        if remaining > 0:
                            response_data += proc.stdout.read(remaining)
                        break
                break

        response = decode_response(response_data)
        if response:
            print(f"Response: {json.dumps(response, indent=2)}")
            if "result" in response and "capabilities" in response["result"]:
                print("✓ Initialize successful!")
            else:
                print("✗ Initialize failed - no capabilities")
        else:
            print("✗ No response received")

        # Send initialized notification
        init_notification = make_notification("initialized", {})
        proc.stdin.write(encode_message(init_notification))
        proc.stdin.flush()

        # Test 2: Open document
        print("\n--- Test 2: Open Document ---")
        test_code = """(define (hello name)
  (string-append "Hello, " name "!"))

(define (main)
  (display (hello "World"))
  (newline))
"""
        did_open = make_notification("textDocument/didOpen", {
            "textDocument": {
                "uri": "file:///tmp/test.ss",
                "languageId": "scheme",
                "version": 1,
                "text": test_code
            }
        })
        proc.stdin.write(encode_message(did_open))
        proc.stdin.flush()
        print("✓ Sent didOpen notification")

        # Wait a bit for diagnostics
        import time
        time.sleep(0.5)

        # Check stderr for logs
        import select
        if select.select([proc.stderr], [], [], 0.1)[0]:
            stderr_data = proc.stderr.read(4096)
            print(f"Server logs:\n{stderr_data.decode('utf-8')}")

        # Test 3: Hover
        print("\n--- Test 3: Hover ---")
        hover_request = make_request(2, "textDocument/hover", {
            "textDocument": {"uri": "file:///tmp/test.ss"},
            "position": {"line": 0, "character": 9}  # Position on 'hello'
        })
        proc.stdin.write(encode_message(hover_request))
        proc.stdin.flush()

        # Read hover response
        response_data = b""
        while True:
            chunk = proc.stdout.read(1)
            if not chunk:
                break
            response_data += chunk
            if b"\r\n\r\n" in response_data:
                header = response_data.split(b"\r\n\r\n")[0].decode('utf-8')
                for line in header.split("\r\n"):
                    if line.startswith("Content-Length:"):
                        length = int(line.split(":")[1].strip())
                        body_start = response_data.find(b"\r\n\r\n") + 4
                        remaining = length - (len(response_data) - body_start)
                        if remaining > 0:
                            response_data += proc.stdout.read(remaining)
                        break
                break

        response = decode_response(response_data)
        if response:
            print(f"Hover response: {json.dumps(response, indent=2)}")

        # Test 4: Completion
        print("\n--- Test 4: Completion ---")
        completion_request = make_request(3, "textDocument/completion", {
            "textDocument": {"uri": "file:///tmp/test.ss"},
            "position": {"line": 4, "character": 3}  # After 'dis'
        })
        proc.stdin.write(encode_message(completion_request))
        proc.stdin.flush()

        # Read completion response
        response_data = b""
        while True:
            chunk = proc.stdout.read(1)
            if not chunk:
                break
            response_data += chunk
            if b"\r\n\r\n" in response_data:
                header = response_data.split(b"\r\n\r\n")[0].decode('utf-8')
                for line in header.split("\r\n"):
                    if line.startswith("Content-Length:"):
                        length = int(line.split(":")[1].strip())
                        body_start = response_data.find(b"\r\n\r\n") + 4
                        remaining = length - (len(response_data) - body_start)
                        if remaining > 0:
                            response_data += proc.stdout.read(remaining)
                        break
                break

        response = decode_response(response_data)
        if response:
            print(f"Completion response (truncated): {json.dumps(response, indent=2)[:500]}...")

        # Test 5: Shutdown
        print("\n--- Test 5: Shutdown ---")
        shutdown_request = make_request(4, "shutdown")
        proc.stdin.write(encode_message(shutdown_request))
        proc.stdin.flush()

        # Read shutdown response
        response_data = b""
        while True:
            chunk = proc.stdout.read(1)
            if not chunk:
                break
            response_data += chunk
            if b"\r\n\r\n" in response_data:
                header = response_data.split(b"\r\n\r\n")[0].decode('utf-8')
                for line in header.split("\r\n"):
                    if line.startswith("Content-Length:"):
                        length = int(line.split(":")[1].strip())
                        body_start = response_data.find(b"\r\n\r\n") + 4
                        remaining = length - (len(response_data) - body_start)
                        if remaining > 0:
                            response_data += proc.stdout.read(remaining)
                        break
                break

        response = decode_response(response_data)
        if response:
            print(f"Shutdown response: {json.dumps(response, indent=2)}")
            print("✓ Shutdown successful!")

        # Send exit notification
        exit_notification = make_notification("exit")
        proc.stdin.write(encode_message(exit_notification))
        proc.stdin.flush()

        # Wait for process to exit
        proc.wait(timeout=5)
        print(f"\nServer exited with code: {proc.returncode}")

    except Exception as e:
        print(f"Error: {e}")
        proc.kill()
        raise
    finally:
        if proc.poll() is None:
            proc.kill()

if __name__ == "__main__":
    test_server()
