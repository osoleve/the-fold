#!/bin/bash
# SFT worker script — runs inside Docker container on each node
# Args: NODE_RANK NNODES MASTER_ADDR MASTER_PORT CONFIG
set -euo pipefail

NODE_RANK=${1:?usage: sft-worker.sh NODE_RANK NNODES MASTER_ADDR MASTER_PORT CONFIG}
NNODES=${2:?}
MASTER_ADDR=${3:?}
MASTER_PORT=${4:?}
CONFIG=${5:?}

echo "[rank $NODE_RANK] Installing dependencies..."
pip install -q mamba-ssm --no-build-isolation 2>&1 | tail -1
pip install -q "git+https://github.com/NVIDIA-NeMo/Automodel.git" wandb torchdata 2>&1 | tail -3

echo "[rank $NODE_RANK] Checking nemo_automodel version..."
python3 -c "import nemo_automodel; print(f'nemo_automodel {nemo_automodel.__version__}')"

echo "[rank $NODE_RANK] Starting training..."
cd /workspace
exec torchrun \
  --nnodes=$NNODES \
  --nproc_per_node=1 \
  --node_rank=$NODE_RANK \
  --master_addr=$MASTER_ADDR \
  --master_port=$MASTER_PORT \
  -m nemo_automodel.recipes.llm.train_ft \
  --config $CONFIG
