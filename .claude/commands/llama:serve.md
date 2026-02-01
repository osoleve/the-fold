---
name: serve
description: Start llama-server with a model
allowed-tools:
  - Bash
  - Read
---

# /llama:serve

Start llama-server with the specified model.

## Usage

```
/llama:serve <model> [options]
```

## Arguments

- `model` - Model name or alias from the registry

## Options

- `--host HOST` - Bind address [default: 127.0.0.1, use 0.0.0.0 for network access]
- `--port PORT` - Listen port [default: 30000]
- `--ctx-size N` - Context size [default: 8192]
- `--gpu-layers N` - GPU layers to offload [default: 99 for full offload]
- `--threads N` - CPU threads [default: 8 for DGX Spark Grace CPU]
- `--no-flash-attn` - Disable flash attention

## Parameter Precedence

CLI flags override model-specific config, which overrides global defaults.

## Instructions

1. Check if arguments were provided. If no model specified, list available models first
2. Run the serve script with the provided arguments
3. Report the result to the user

## Execution

```bash
~/llama-spark-plugin/scripts/llama-serve.sh $ARGUMENTS
```

After starting, verify with a health check:
```bash
curl -s http://localhost:30000/health
```
