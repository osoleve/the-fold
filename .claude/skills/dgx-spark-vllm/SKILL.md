---
name: dgx-spark-vllm
description: DGX Spark vLLM optimization guidance and best practices
---

# DGX Spark vLLM Optimization

Guidance for running LLMs via vLLM on NVIDIA DGX Spark (Grace-Blackwell architecture).

## Hardware Overview

- **CPU**: Grace ARM64 (72 cores)
- **GPU**: Blackwell GPU
- **Memory**: 119GB unified memory (GPU can access system RAM)
- **Architecture**: GB10 (ARM + Blackwell unified)

## vLLM Server

The DGX Spark runs vLLM as a persistent service on port 8000. Use the MCP tools or curl directly:

```bash
# Check status
curl http://localhost:8000/v1/models

# Chat completion
curl http://localhost:8000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model": "MODEL_NAME", "messages": [{"role": "user", "content": "Hello"}]}'
```

## MCP Tools

The `vllm` MCP server provides:
- `vllm_status` - Check vLLM server status and list models
- `vllm_chat` - Chat completion with optional model selection
- `vllm_complete` - Raw text completion

Set `VLLM_URL` environment variable to override default `http://localhost:8000`.

## Model Selection

vLLM dynamically loads models from HuggingFace. Current models available via `/vllm:models`.

### Recommended for DGX Spark

| Use Case | Model | Notes |
|----------|-------|-------|
| General | Qwen3-Next-80B-A3B | MoE architecture, efficient |
| Coding | Qwen2.5-Coder-32B | Strong coding performance |
| Fast | Llama-3.2-3B | Quick responses |

## Performance Tuning

### Context Size
vLLM handles context automatically. Check `max_model_len` in model info:
```bash
curl -s http://localhost:8000/v1/models | jq '.data[0].max_model_len'
```

### Tensor Parallelism
For multi-GPU setups, vLLM uses tensor parallelism automatically.

### KV Cache
vLLM manages KV cache efficiently with PagedAttention. No manual tuning needed.

## Troubleshooting

### Server Not Responding
```bash
# Check service status
systemctl status vllm

# View logs
journalctl -u vllm -f
```

### Out of Memory
- Use FP8 quantized models (e.g., `-FP8` suffix)
- Reduce `max_model_len` in vLLM config
- Use smaller models

### Slow First Request
First request warms up CUDA kernels. Subsequent requests are faster.
