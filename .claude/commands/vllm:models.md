---
name: models
description: List available models on vLLM server
allowed-tools:
  - Bash
---

# /llama:models

List all models currently loaded on the vLLM server.

## Instructions

Query the vLLM server's `/v1/models` endpoint:

```bash
VLLM_URL="${VLLM_URL:-http://localhost:8000}"

curl -s "$VLLM_URL/v1/models" | jq -r '
  .data[] |
  "Model: \(.id)\n  Max context: \(.max_model_len)\n  Owner: \(.owned_by)\n"
' 2>/dev/null || echo "Could not reach vLLM server at $VLLM_URL"
```

This will show:
- Model ID (HuggingFace model name)
- Maximum context length
- Owner (typically "vllm")
