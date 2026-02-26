#!/usr/bin/env python3
"""HTTP relay for RLM LLM calls.

Long-lived subprocess that maintains persistent HTTP sessions.
Supports both sequential and concurrent (batched) requests.

Protocol: JSON lines over stdin/stdout.

  Single request:
    {"url": "...", "headers": {...}, "body": {...}, "timeout": 300}
    -> {"status": 200, "body": "..."}

  Batch request (concurrent):
    {"batch": [{"id": "r1", "url": "...", "headers": {...}, "body": {...}}, ...], "timeout": 300}
    -> {"batch": [{"id": "r1", "status": 200, "body": "..."}, ...]}

Batch requests are executed concurrently via aiohttp with a configurable
concurrency window (default: 16). Responses are returned in the same order
as requests.

Exits cleanly on EOF (stdin closed) or SIGTERM.
"""

import asyncio
import json
import signal
import sys

import aiohttp
import requests


# --- Async batch handler ---

async def _do_one(session, req, semaphore):
    """Execute a single HTTP request under a concurrency semaphore."""
    rid = req.get("id", "?")
    url = req.get("url", "")
    headers = req.get("headers", {})
    body = req.get("body", {})
    timeout = req.get("timeout", 300)
    method = req.get("method", "POST").upper()

    async with semaphore:
        try:
            if method == "POST":
                async with session.post(
                    url, headers=headers, json=body,
                    timeout=aiohttp.ClientTimeout(total=timeout)
                ) as r:
                    text = await r.text()
                    return {"id": rid, "status": r.status, "body": text}
            else:
                async with session.get(
                    url, headers=headers,
                    timeout=aiohttp.ClientTimeout(total=timeout)
                ) as r:
                    text = await r.text()
                    return {"id": rid, "status": r.status, "body": text}
        except asyncio.TimeoutError:
            return {"id": rid, "status": -1, "error": f"Timeout after {timeout}s"}
        except aiohttp.ClientError as e:
            return {"id": rid, "status": -1, "error": f"Connection error: {e}"}
        except Exception as e:
            return {"id": rid, "status": -1, "error": f"Request error: {e}"}


async def handle_batch(items, timeout, concurrency):
    """Execute a batch of requests concurrently."""
    sem = asyncio.Semaphore(concurrency)
    # Propagate batch-level timeout to individual requests if not set
    for item in items:
        if "timeout" not in item:
            item["timeout"] = timeout

    connector = aiohttp.TCPConnector(limit=concurrency, keepalive_timeout=30)
    async with aiohttp.ClientSession(
        connector=connector,
        headers={"Content-Type": "application/json"}
    ) as session:
        tasks = [_do_one(session, req, sem) for req in items]
        results = await asyncio.gather(*tasks)
    return list(results)


# --- Sync single-request handler (legacy) ---

def handle_single(session, req):
    url = req.get("url", "")
    headers = req.get("headers", {})
    body = req.get("body", {})
    timeout = req.get("timeout", 300)
    method = req.get("method", "POST").upper()

    try:
        if method == "POST":
            r = session.post(url, headers=headers, json=body, timeout=timeout)
        else:
            r = session.get(url, headers=headers, timeout=timeout)
        return {"status": r.status_code, "body": r.text}
    except requests.Timeout:
        return {"status": -1, "error": f"Timeout after {timeout}s"}
    except requests.ConnectionError as e:
        return {"status": -1, "error": f"Connection error: {e}"}
    except Exception as e:
        return {"status": -1, "error": f"Request error: {e}"}


# --- Main loop ---

def main():
    session = requests.Session()
    session.headers.update({"Content-Type": "application/json"})

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

        if "batch" in req:
            # Concurrent batch mode
            items = req["batch"]
            timeout = req.get("timeout", 300)
            concurrency = req.get("concurrency", 16)
            results = asyncio.run(handle_batch(items, timeout, concurrency))
            sys.stdout.write(json.dumps({"batch": results}) + "\n")
            sys.stdout.flush()
        else:
            # Single request (legacy compatible)
            resp = handle_single(session, req)
            sys.stdout.write(json.dumps(resp) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
