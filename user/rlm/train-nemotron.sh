#!/bin/bash
# Multi-node SFT training for Nemotron-3-Nano on 2x DGX Spark
# Uses NeMo AutoModel with FSDP2 + Expert Parallelism
#
# Usage: ./user/rlm/train-nemotron.sh

set -euo pipefail

MASTER_ADDR=192.168.100.10
MASTER_PORT=29500
NNODES=2
NCCL_IF=enp1s0f0np0
CONFIG=user/rlm/nemotron_sft.yaml

DOCKER_IMAGE="nvcr.io/nvidia/vllm:26.01-py3"

DOCKER_ARGS="--gpus all --network host --ipc=host --shm-size=10g \
  -v /home/osoleve/.cache/huggingface:/root/.cache/huggingface \
  -v /home/osoleve/fold:/workspace \
  -v /home/osoleve/.netrc:/root/.netrc:ro \
  -e NCCL_SOCKET_IFNAME=$NCCL_IF \
  -e GLOO_SOCKET_IFNAME=$NCCL_IF \
  -e TP_SOCKET_IFNAME=$NCCL_IF \
  -e NCCL_DEBUG=INFO \
  -e MASTER_ADDR=$MASTER_ADDR \
  -e MASTER_PORT=$MASTER_PORT"

echo "=== Nemotron-3-Nano SFT (2-node) ==="
echo "Master: $MASTER_ADDR:$MASTER_PORT"
echo "NCCL interface: $NCCL_IF"
echo ""

# Launch node 1 (elsie-2) in background via SSH
# Worker script is at /workspace/user/rlm/sft-worker.sh inside container
echo "[node 1] Starting on elsie-2..."
ssh 192.168.100.11 "docker run --rm $DOCKER_ARGS --name sft-node1 $DOCKER_IMAGE bash /workspace/user/rlm/sft-worker.sh 1 $NNODES $MASTER_ADDR $MASTER_PORT $CONFIG" &
NODE1_PID=$!

# Small delay to let node 1 start
sleep 5

# Launch node 0 (elsie-1) in foreground
echo "[node 0] Starting on elsie-1..."
docker run --rm $DOCKER_ARGS --name sft-node0 $DOCKER_IMAGE bash /workspace/user/rlm/sft-worker.sh 0 $NNODES $MASTER_ADDR $MASTER_PORT $CONFIG

# Wait for node 1
wait $NODE1_PID
echo ""
echo "=== Training complete ==="
