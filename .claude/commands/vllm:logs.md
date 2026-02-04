---
name: logs
description: View vLLM server logs
arguments: "[lines]"
allowed-tools:
  - Bash
---

# /llama:logs

View vLLM server logs.

## Usage

```
/llama:logs [lines]
```

## Arguments

- `lines` - Number of lines to show (default: 50)

## Instructions

View logs based on how vLLM is running:

```bash
LINES="${1:-50}"

# If running as systemd service
echo "=== vLLM Service Logs ==="
journalctl -u vllm -n "$LINES" --no-pager 2>/dev/null || echo "(systemd service not found)"

# If running as docker container
echo ""
echo "=== Docker Logs (if applicable) ==="
docker logs --tail "$LINES" vllm 2>/dev/null || echo "(docker container not found)"
```

## Manual Process Logs

If vLLM is running in foreground, logs go to stdout/stderr of that terminal.
