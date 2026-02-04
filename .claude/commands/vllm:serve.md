---
name: serve
description: Information about vLLM server management
allowed-tools:
  - Bash
---

# /llama:serve

**Note:** vLLM runs as a persistent system service on DGX Spark.

## Current Status

Check if vLLM is running:

```bash
VLLM_URL="${VLLM_URL:-http://localhost:8000}"
curl -s "$VLLM_URL/v1/models" | jq -r '.data[].id' 2>/dev/null && echo "vLLM is running" || echo "vLLM is not responding"
```

## Service Management

vLLM is managed via systemd (if configured) or docker. Common operations:

```bash
# Check service status
systemctl status vllm

# Start/stop/restart (requires appropriate permissions)
sudo systemctl start vllm
sudo systemctl stop vllm
sudo systemctl restart vllm

# View logs
journalctl -u vllm -f
```

## Manual Startup

If vLLM is not running as a service, start it manually:

```bash
# Example with Qwen model
vllm serve "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" \
  --host 0.0.0.0 \
  --port 8000 \
  --tensor-parallel-size 1
```

## Environment Variables

Set `VLLM_URL` to override the default endpoint:
```bash
export VLLM_URL="http://localhost:8000"
```
