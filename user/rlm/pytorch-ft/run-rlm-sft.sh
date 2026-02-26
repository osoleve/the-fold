#!/bin/bash
# Launch RLM SFT on 2x DGX Spark via Docker Swarm
# Requires: docker stack finetuning-multinode already deployed
set -euo pipefail

HF_TOKEN=$(cat ~/.cache/huggingface/token)

# Get container IDs
LOCAL_CONTAINER=$(docker ps -q -f name=finetuning-multinode)
REMOTE_CONTAINER=$(ssh 192.168.100.11 "docker ps -q -f name=finetuning-multinode")

echo "Local container (rank 0): $LOCAL_CONTAINER"
echo "Remote container (rank 1): $REMOTE_CONTAINER"

TRAIN_CMD='bash /workspace/install-requirements 2>&1 | tail -1;
  pip install wandb 2>&1 | tail -1;
  accelerate launch --config_file=/workspace/configs/CONFIG_FILE /workspace/rlm_sft.py'

# Launch rank 1 (elsie-2) in background
echo "[rank 1] Starting on elsie-2..."
ssh 192.168.100.11 "docker exec \
  -e HF_TOKEN=$HF_TOKEN \
  -e WANDB_API_KEY=\$(cat /root/.netrc 2>/dev/null | grep -A2 'api.wandb.ai' | grep password | awk '{print \$2}' || echo '') \
  $REMOTE_CONTAINER bash -c '$(echo "$TRAIN_CMD" | sed "s/CONFIG_FILE/config_finetuning_rank1.yaml/")'" &
RANK1_PID=$!

sleep 5

# Launch rank 0 (elsie-1) in foreground
echo "[rank 0] Starting on elsie-1..."
docker exec \
  -e HF_TOKEN=$HF_TOKEN \
  $LOCAL_CONTAINER bash -c "$(echo "$TRAIN_CMD" | sed 's/CONFIG_FILE/config_finetuning.yaml/')"

wait $RANK1_PID
echo "=== Training complete ==="
