#!/usr/bin/env python3
"""HTTP relay for RLM LLM calls.

Long-lived subprocess that maintains a persistent HTTP session with
keep-alive. Eliminates per-call subprocess spawn overhead (~2-5s per curl).

Protocol: JSON lines over stdin/stdout.
  Request:  {"url": "...", "headers": {...}, "body": {...}, "timeout": 300}
  Response: {"status": 200, "body": "..."} or {"status": -1, "error": "..."}

Exits cleanly on EOF (stdin closed) or SIGTERM.
"""

import json
import signal
import sys

import requests

def main():
    session = requests.Session()
    # HTTP keep-alive is default in requests.Session
    session.headers.update({"Content-Type": "application/json"})

    # Clean shutdown on SIGTERM
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except json.JSONDecodeError as e:
            resp = {"status": -1, "error": f"JSON parse error: {e}"}
            sys.stdout.write(json.dumps(resp) + "\n")
            sys.stdout.flush()
            continue

        url = req.get("url", "")
        headers = req.get("headers", {})
        body = req.get("body", {})
        timeout = req.get("timeout", 300)
        method = req.get("method", "POST").upper()

        try:
            if method == "POST":
                r = session.post(url, headers=headers,
                                 json=body, timeout=timeout)
            else:
                r = session.get(url, headers=headers, timeout=timeout)

            resp = {"status": r.status_code, "body": r.text}
        except requests.Timeout:
            resp = {"status": -1, "error": f"Timeout after {timeout}s"}
        except requests.ConnectionError as e:
            resp = {"status": -1, "error": f"Connection error: {e}"}
        except Exception as e:
            resp = {"status": -1, "error": f"Request error: {e}"}

        sys.stdout.write(json.dumps(resp) + "\n")
        sys.stdout.flush()


if __name__ == "__main__":
    main()
