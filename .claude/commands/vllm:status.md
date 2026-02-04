---
name: status
description: Show vLLM server status
allowed-tools:
  - Bash
---

# /llama:status

Show the current status of the vLLM server on DGX Spark.

## Instructions

1. Query the vLLM server's `/v1/models` endpoint to check availability
2. Display available models and server info
3. Report status to user

## Execution

```bash
VLLM_URL="${VLLM_URL:-http://localhost:8000}"

echo "Checking vLLM server at $VLLM_URL..."
echo ""

# Check models endpoint
MODELS=$(curl -s "$VLLM_URL/v1/models" 2>/dev/null)
if [ $? -eq 0 ] && [ -n "$MODELS" ]; then
    echo "Status: RUNNING"
    echo ""
    echo "Available models:"
    echo "$MODELS" | jq -r '.data[].id' 2>/dev/null || echo "  (could not parse model list)"
    echo ""
    echo "Server info:"
    echo "$MODELS" | jq -r '.data[0] | "  Max context: \(.max_model_len)"' 2>/dev/null
else
    echo "Status: NOT RESPONDING"
    echo ""
    echo "vLLM server is not running or not reachable at $VLLM_URL"
    echo "Check with: systemctl status vllm (if running as service)"
fi
```
