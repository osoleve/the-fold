---
name: download
description: Pre-download a model from HuggingFace for vLLM
arguments: "<repo>"
allowed-tools:
  - Bash
---

# /llama:download

Pre-download a model from HuggingFace Hub for faster vLLM startup.

## Usage

```
/llama:download <repo>
```

## Arguments

- `repo` - HuggingFace repository (e.g., `Qwen/Qwen3-Next-80B-A3B-Instruct-FP8`)

## Note

vLLM automatically downloads models on first use. This command is for pre-caching.

## Examples

```
/llama:download Qwen/Qwen2.5-Coder-32B-Instruct
/llama:download meta-llama/Llama-3.1-70B-Instruct
```

## Instructions

Use the HuggingFace CLI to pre-download:

```bash
REPO="${1:?Usage: /llama:download <repo>}"

echo "Downloading $REPO..."
huggingface-cli download "$REPO" --local-dir-use-symlinks False

echo ""
echo "Model cached. Use in vLLM with: --model $REPO"
```

## FP8 Quantized Models

For DGX Spark, prefer FP8 quantized versions when available:
- Suffix `-FP8` indicates FP8 quantization
- Better memory efficiency with minimal quality loss

Example: `Qwen/Qwen3-Next-80B-A3B-Instruct-FP8`
