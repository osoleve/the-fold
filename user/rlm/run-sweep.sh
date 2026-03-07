#!/usr/bin/env bash
# Launch Optuna SFT sweep in Docker container.
# Usage: ./run-sweep.sh --model MODEL_NAME [--trials N] [--max-steps N] [--study-name NAME]
#
# Uses shared NFS at /shared/rl-outputs/optuna for Optuna study DB.
# Both machines contribute trials to the same study — TPE sees all results.

set -euo pipefail

MODEL=""
TRIALS=5
MAX_STEPS=200
HOST_TAG=$(hostname -s)
IMAGE="fold-sft:latest"
STUDY_NAME=""
NO_LORA=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --model)      MODEL="$2"; shift 2 ;;
    --trials)     TRIALS="$2"; shift 2 ;;
    --max-steps)  MAX_STEPS="$2"; shift 2 ;;
    --study-name) STUDY_NAME="$2"; shift 2 ;;
    --host-tag)   HOST_TAG="$2"; shift 2 ;;
    --image)      IMAGE="$2"; shift 2 ;;
    --no-lora)    NO_LORA="--no-lora"; shift ;;
    *) echo "Unknown arg: $1" >&2; exit 1 ;;
  esac
done

if [[ -z "$MODEL" ]]; then
  echo "Error: --model is required (e.g. Qwen3.5-4B-Base-TextOnly)" >&2
  exit 1
fi

# Derive study name from model if not specified
if [[ -z "$STUDY_NAME" ]]; then
  STUDY_NAME="fluency-sft-$(echo "$MODEL" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g')"
fi

MODEL_PATH="/models/${MODEL}"
SHARED_DB="/shared/rl-outputs/optuna/${STUDY_NAME}.db"
SWEEP_DIR="$HOME/fold/user/rlm/sweeps/${STUDY_NAME}/${HOST_TAG}"
mkdir -p "$SWEEP_DIR"

WANDB_KEY=$(grep password ~/.netrc 2>/dev/null | awk '{print $2}')

echo "=== SFT Sweep ==="
echo "  Model:      $MODEL"
echo "  Host:       $HOST_TAG"
echo "  Trials:     $TRIALS"
echo "  Max steps:  $MAX_STEPS"
echo "  Study DB:   $SHARED_DB (NFS)"
echo "  Study name: $STUDY_NAME"
echo "  Output dir: $SWEEP_DIR"
echo ""

exec docker run --rm --gpus all \
  --ipc=host \
  --ulimit memlock=-1 \
  --ulimit stack=67108864 \
  -v /home/osoleve/models:/models:ro \
  -v /home/osoleve/fold:/workspace/fold \
  -v /shared:/shared \
  -v "${SWEEP_DIR}:/workspace/sweep" \
  -e WANDB_API_KEY="${WANDB_KEY}" \
  -e TOKENIZERS_PARALLELISM=false \
  -w /workspace/fold/user/rlm \
  "$IMAGE" \
  python3 train-sft-base.py \
    --model "$MODEL_PATH" \
    --data-dir /workspace/fold/user/rlm/training/base \
    --output-dir /workspace/sweep/outputs \
    --optuna \
    --study-db "$SHARED_DB" \
    --study-name "$STUDY_NAME" \
    --n-trials "$TRIALS" \
    --max-steps "$MAX_STEPS" \
    --run-name "sweep-${HOST_TAG}" \
    $NO_LORA
