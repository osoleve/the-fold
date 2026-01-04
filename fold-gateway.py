#!/usr/bin/env python3
"""
fold-gateway.py — Unix Domain Socket Gateway for The Fold REPL.

Acts as a supervisor for Scheme worker processes, exposing a JSON-RPC-like
interface over a Unix Domain Socket.

Architecture:
  [Client] <--> [Gateway (Python)] <--> [Worker (Scheme)]

Protocol:
  Client -> Gateway: JSON line {"id": "...", "op": "eval", "session": "...", "code": "..."}
  Gateway -> Client: JSON lines (streamed)
    {"id": "...", "type": "stdout", "chunk": "..."}
    {"id": "...", "type": "result", "value": "...", "cost": ...}
"""

import asyncio
import json
import os
import sys
import time
import signal
import shutil
import logging

# Configuration
SOCKET_PATH = ".fold-repl/fold.sock"
WORKER_SCRIPT = "shell/worker-stdio.ss"
WORKER_IDLE_TIMEOUT = 300  # Kill workers after 5 minutes of inactivity

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler(sys.stderr)]
)
logger = logging.getLogger("gateway")

class Worker:
    def __init__(self, session_id):
        self.session_id = session_id
        self.process = None
        self.last_active = time.time()
        self.lock = asyncio.Lock()

    async def start(self):
        logger.info(f"Starting worker for session {self.session_id}")
        self.process = await asyncio.create_subprocess_exec(
            "scheme", "--script", WORKER_SCRIPT,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            limit=1024*1024 # 1MB buffer
        )
        # Start reading stderr in background
        asyncio.create_task(self._read_stderr())

    async def _read_stderr(self):
        while self.process and self.process.returncode is None:
            line = await self.process.stderr.readline()
            if not line:
                break
            logger.warning(f"Worker {self.session_id} stderr: {line.decode().strip()}")

    async def send_request(self, request):
        if not self.process or self.process.returncode is not None:
            await self.start()
        
        self.last_active = time.time()
        
        # Format for Scheme worker
        # request is like {"id": "...", "code": "..."}
        # We assume the worker expects exactly this structure
        payload = json.dumps(request) + "\n"
        try:
            self.process.stdin.write(payload.encode())
            await self.process.stdin.drain()
        except BrokenPipeError:
            logger.error(f"Worker {self.session_id} died, restarting...")
            await self.start()
            self.process.stdin.write(payload.encode())
            await self.process.stdin.drain()

    async def kill(self):
        if self.process and self.process.returncode is None:
            logger.info(f"Killing worker {self.session_id}")
            try:
                self.process.terminate()
                await self.process.wait()
            except ProcessLookupError:
                pass

class GatewayServer:
    def __init__(self):
        self.workers = {}  # session_id -> Worker
        self.cleanup_task = None

    async def handle_client(self, reader, writer):
        client_addr = writer.get_extra_info('peername')
        logger.info(f"New client connection")

        try:
            while True:
                line = await reader.readline()
                if not line:
                    break
                
                try:
                    request = json.loads(line)
                except json.JSONDecodeError:
                    writer.write(json.dumps({"type": "error", "message": "Invalid JSON"}).encode() + b"\n")
                    await writer.drain()
                    continue

                session_id = request.get("session", "default")
                req_id = request.get("id", "unknown")
                code = request.get("code")

                if not code:
                    writer.write(json.dumps({"id": req_id, "type": "error", "message": "Missing 'code'"}).encode() + b"\n")
                    await writer.drain()
                    continue

                worker = self._get_worker(session_id)
                
                # Forward to worker
                # We construct the payload the worker expects: {"id": "...", "code": "..."}
                worker_payload = {"id": req_id, "code": code}
                
                async with worker.lock:
                    await worker.send_request(worker_payload)
                    
                    # Read responses until we get a result or error
                    while True:
                        if not worker.process or worker.process.returncode is not None:
                             writer.write(json.dumps({"id": req_id, "type": "error", "message": "Worker crashed"}).encode() + b"\n")
                             break

                        worker_line = await worker.process.stdout.readline()
                        if not worker_line:
                            writer.write(json.dumps({"id": req_id, "type": "error", "message": "Worker EOF"}).encode() + b"\n")
                            break
                        
                        # Forward worker output directly to client
                        writer.write(worker_line)
                        await writer.drain()

                        # Check if this was the final message
                        try:
                            response = json.loads(worker_line)
                            if response.get("id") == req_id and response.get("type") in ["result", "error"]:
                                break
                        except json.JSONDecodeError:
                            pass # Should not happen if worker is correct

        except ConnectionResetError:
            pass
        finally:
            logger.info("Client disconnected")
            writer.close()
            await writer.wait_closed()

    def _get_worker(self, session_id):
        if session_id not in self.workers:
            self.workers[session_id] = Worker(session_id)
        return self.workers[session_id]

    async def cleanup_loop(self):
        while True:
            await asyncio.sleep(60)
            now = time.time()
            to_remove = []
            for sid, worker in self.workers.items():
                if now - worker.last_active > WORKER_IDLE_TIMEOUT:
                    await worker.kill()
                    to_remove.append(sid)
            
            for sid in to_remove:
                del self.workers[sid]

    async def start(self):
        if os.path.exists(SOCKET_PATH):
            os.unlink(SOCKET_PATH)
        
        # Ensure directory exists
        os.makedirs(os.path.dirname(SOCKET_PATH), exist_ok=True)

        server = await asyncio.start_unix_server(
            self.handle_client, path=SOCKET_PATH
        )
        
        # Set permissions so others can connect if needed (default is usually fine for user)
        # os.chmod(SOCKET_PATH, 0o600) 

        logger.info(f"Gateway listening on {SOCKET_PATH}")
        
        self.cleanup_task = asyncio.create_task(self.cleanup_loop())

        async with server:
            await server.serve_forever()

if __name__ == "__main__":
    gateway = GatewayServer()
    try:
        asyncio.run(gateway.start())
    except KeyboardInterrupt:
        logger.info("Gateway stopping...")
