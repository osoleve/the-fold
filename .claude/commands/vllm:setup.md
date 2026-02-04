---
name: setup
description: Set up vLLM on DGX Spark
allowed-tools:
  - Bash
---

# /llama:setup

Guide for setting up vLLM on NVIDIA DGX Spark.

## Prerequisites

- NVIDIA DGX Spark with Grace-Blackwell architecture
- CUDA toolkit 12.1+ installed
- Python 3.9+

## Instructions

Check if vLLM is already installed:

```bash
python3 -c "import vllm; print(f'vLLM {vllm.__version__} installed')" 2>/dev/null || echo "vLLM not installed"
```

### Installation Steps

1. **Install vLLM with CUDA support**:
```bash
pip install vllm
```

For specific CUDA versions:
```bash
# CUDA 12.1
pip install vllm --extra-index-url https://download.pytorch.org/whl/cu121
```

2. **Verify installation**:
```bash
python3 -c "import vllm; print(vllm.__version__)"
```

3. **Test with a small model**:
```bash
vllm serve "Qwen/Qwen2.5-1.5B-Instruct" --port 8000 &
sleep 30
curl http://localhost:8000/v1/models
```

## Running as a Service

For persistent deployment, create a systemd service:

```bash
sudo tee /etc/systemd/system/vllm.service << 'EOF'
[Unit]
Description=vLLM Inference Server
After=network.target

[Service]
Type=simple
User=$USER
ExecStart=/usr/bin/vllm serve "MODEL_NAME" --host 0.0.0.0 --port 8000
Restart=always

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl enable vllm
sudo systemctl start vllm
```

## Post-Installation

Run `/llama:models` to see loaded models, or `/llama:status` to check server health.
