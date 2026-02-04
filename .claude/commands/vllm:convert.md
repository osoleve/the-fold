---
name: convert
description: Information about vLLM quantization options
allowed-tools:
  - Bash
---

# /llama:convert

**Note:** vLLM loads HuggingFace models directly. No GGUF conversion needed.

## vLLM Quantization Options

vLLM supports several quantization methods at load time:

| Method | Flag | Notes |
|--------|------|-------|
| FP8 | `--quantization fp8` | Best for Blackwell GPUs |
| AWQ | `--quantization awq` | Requires AWQ-quantized model |
| GPTQ | `--quantization gptq` | Requires GPTQ-quantized model |
| INT8 | `--quantization int8` | Dynamic quantization |

## Using Pre-Quantized Models

Many models have pre-quantized versions on HuggingFace:

```bash
# FP8 quantized (recommended for DGX Spark)
vllm serve "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" --port 8000

# AWQ quantized
vllm serve "TheBloke/Llama-2-70B-AWQ" --quantization awq --port 8000
```

## DGX Spark Recommendation

With Blackwell GPU, prefer FP8 quantization:
- Native hardware support
- Minimal quality loss
- Best memory efficiency

Look for models with `-FP8` suffix on HuggingFace.

## Creating AWQ Quantized Models

If needed, use AutoAWQ:

```bash
pip install autoawq
python -c "
from awq import AutoAWQForCausalLM
model = AutoAWQForCausalLM.from_pretrained('model-name')
model.quantize(tokenizer, quant_config={'w_bit': 4})
model.save_quantized('model-awq')
"
```
