#!/usr/bin/env python3
"""
fold-agent.py — JSON-Socket Client for The Fold REPL.

Connects to the Fold Gateway socket, sends requests, and prints responses.
Supports streaming output.
"""

import socket
import json
import argparse
import sys
import uuid
import os

SOCKET_PATH = ".fold-repl/fold.sock"

def generate_request_id():
    return f"req-{uuid.uuid4().hex[:8]}"

def main():
    parser = argparse.ArgumentParser(description="Fold REPL Agent Client")
    parser.add_argument("code", nargs="?", help="Code to execute")
    parser.add_argument("--session", help="Session ID", default=f"agent-{uuid.uuid4().hex[:8]}")
    parser.add_argument("--json", action="store_true", help="Read/Write JSON to stdin/stdout")
    parser.add_argument("--raw", action="store_true", help="Just print result value")
    
    args = parser.parse_args()
    
    code = args.code
    session_id = args.session

    if args.json and not sys.stdin.isatty():
        try:
            data = json.load(sys.stdin)
            code = data.get("code", code)
            session_id = data.get("session", session_id)
        except json.JSONDecodeError as e:
            print(json.dumps({"status": "error", "error": f"Invalid JSON input: {e}"}))
            return

    if not code:
        print(json.dumps({"status": "error", "error": "No code provided"}))
        return

    if not os.path.exists(SOCKET_PATH):
        print(json.dumps({"status": "error", "error": f"Gateway socket not found at {SOCKET_PATH}. Is fold-gateway.py running?"}))
        return

    req_id = generate_request_id()
    request = {
        "id": req_id,
        "session": session_id,
        "code": code
    }

    try:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.connect(SOCKET_PATH)
        
        # Send request
        client.sendall((json.dumps(request) + "\n").encode())
        
        # Read response
        f = client.makefile('r')
        final_result = None
        output_buffer = []

        for line in f:
            try:
                resp = json.loads(line)
                if resp.get("id") != req_id:
                    continue
                
                msg_type = resp.get("type")
                
                if msg_type == "stdout":
                    chunk = resp.get("chunk", "")
                    output_buffer.append(chunk)
                    if not args.json and not args.raw:
                        sys.stderr.write(chunk)
                        sys.stderr.flush()
                
                elif msg_type == "stderr":
                    chunk = resp.get("chunk", "")
                    output_buffer.append(chunk)
                    if not args.json and not args.raw:
                        sys.stderr.write(chunk)
                        sys.stderr.flush()

                elif msg_type == "result":
                    final_result = resp.get("value")
                    break
                
                elif msg_type == "error":
                    final_result = f"Error: {resp.get('message')}"
                    break

            except json.JSONDecodeError:
                pass
        
        client.close()

        full_output = "".join(output_buffer)

        if args.json:
            print(json.dumps({
                "status": "success" if not str(final_result).startswith("Error:") else "error",
                "result": final_result,
                "output": full_output,
                "session": session_id
            }))
        elif args.raw:
            print(final_result)
        else:
            if final_result:
                print(final_result)

    except Exception as e:
        print(json.dumps({"status": "error", "error": str(e)}))

if __name__ == "__main__":
    main()