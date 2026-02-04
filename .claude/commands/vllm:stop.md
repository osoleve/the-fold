---
name: stop
description: Stop the vLLM server
allowed-tools:
  - Bash
---

# /llama:stop

Stop the vLLM server (if running as a manageable service).

## Usage

```
/llama:stop
```

## Instructions

vLLM typically runs as a persistent service. Stopping options:

```bash
# If running as systemd service
sudo systemctl stop vllm

# If running as a docker container
docker stop vllm

# If running in foreground, use Ctrl+C or:
pkill -f "vllm serve"
```

**Note:** Stopping vLLM will interrupt all ongoing inference requests. Ensure no critical operations are in progress.
