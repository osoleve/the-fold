#!/usr/bin/env python3
"""SFT fine-tune on RLM v2 trajectories.

Usage (inside Docker container with GPU):
  pip install trl datasets wandb
  python3 train-sft.py [--model MODEL] [--epochs N] [--lr LR] [--run-name NAME] [--no-wandb]

Output: ./models/<model-slug>-rlm-sft/
"""

import os
import json
import argparse
from datasets import load_dataset
from transformers import AutoTokenizer, AutoModelForCausalLM
from trl import SFTTrainer, SFTConfig

# --- Paths ---
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
TRAIN_PATH = os.path.join(SCRIPT_DIR, "training", "train.jsonl")
EVAL_PATH = os.path.join(SCRIPT_DIR, "training", "eval.jsonl")

# --- Defaults ---
DEFAULT_MODEL = "Qwen/Qwen3-0.6B"
DEFAULT_MAX_SEQ_LEN = 2048
DEFAULT_LR = 2e-5
DEFAULT_EPOCHS = 3
DEFAULT_BATCH_SIZE = 4
DEFAULT_GRAD_ACCUM = 4  # effective batch size = 16
DEFAULT_WARMUP_RATIO = 0.10
DEFAULT_WEIGHT_DECAY = 0.01
WANDB_PROJECT = "fold-rlm-sft"


def parse_args():
    p = argparse.ArgumentParser(description="SFT fine-tune for RLM v2")
    p.add_argument("--model", default=DEFAULT_MODEL, help="Base model (HF hub ID or local path)")
    p.add_argument("--epochs", type=int, default=DEFAULT_EPOCHS)
    p.add_argument("--lr", type=float, default=DEFAULT_LR)
    p.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    p.add_argument("--grad-accum", type=int, default=DEFAULT_GRAD_ACCUM)
    p.add_argument("--max-seq-len", type=int, default=DEFAULT_MAX_SEQ_LEN)
    p.add_argument("--warmup-ratio", type=float, default=DEFAULT_WARMUP_RATIO)
    p.add_argument("--weight-decay", type=float, default=DEFAULT_WEIGHT_DECAY)
    p.add_argument("--run-name", default=None, help="W&B run name (auto-generated if omitted)")
    p.add_argument("--no-wandb", action="store_true", help="Disable W&B logging")
    p.add_argument("--output-dir", default=None, help="Override output directory")
    return p.parse_args()


def model_slug(model_id):
    """Qwen/Qwen3-0.6B -> qwen3-0.6b"""
    return model_id.split("/")[-1].lower()


def main():
    args = parse_args()

    slug = model_slug(args.model)
    output_dir = args.output_dir or os.path.join(SCRIPT_DIR, "models", f"{slug}-rlm-sft")
    run_name = args.run_name or f"{slug}-sft-e{args.epochs}-lr{args.lr}"

    # --- W&B setup ---
    use_wandb = not args.no_wandb
    if use_wandb:
        try:
            import wandb
            wandb.init(
                project=WANDB_PROJECT,
                name=run_name,
                config={
                    "base_model": args.model,
                    "epochs": args.epochs,
                    "lr": args.lr,
                    "batch_size": args.batch_size,
                    "grad_accum": args.grad_accum,
                    "effective_batch_size": args.batch_size * args.grad_accum,
                    "max_seq_len": args.max_seq_len,
                    "warmup_ratio": args.warmup_ratio,
                    "weight_decay": args.weight_decay,
                    "train_file": TRAIN_PATH,
                    "eval_file": EVAL_PATH,
                },
            )
            print(f"W&B run: {wandb.run.url}")
        except ImportError:
            print("wandb not installed, falling back to no logging")
            use_wandb = False

    print(f"Loading tokenizer from {args.model}...")
    tokenizer = AutoTokenizer.from_pretrained(args.model, trust_remote_code=True)

    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    print(f"Loading model from {args.model}...")
    model = AutoModelForCausalLM.from_pretrained(
        args.model,
        dtype="bfloat16",
        trust_remote_code=True,
    )

    print(f"Loading datasets...")
    dataset = load_dataset("json", data_files={
        "train": TRAIN_PATH,
        "eval": EVAL_PATH,
    })
    print(f"  train: {len(dataset['train'])}, eval: {len(dataset['eval'])}")

    # --- Training config ---
    training_args = SFTConfig(
        output_dir=output_dir,
        run_name=run_name,
        num_train_epochs=args.epochs,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        gradient_accumulation_steps=args.grad_accum,
        learning_rate=args.lr,
        warmup_ratio=args.warmup_ratio,
        weight_decay=args.weight_decay,
        bf16=True,
        logging_steps=10,
        eval_strategy="steps",
        eval_steps=50,
        save_strategy="steps",
        save_steps=100,
        save_total_limit=2,
        max_length=args.max_seq_len,
        report_to="wandb" if use_wandb else "none",
        dataloader_num_workers=2,
        gradient_checkpointing=True,
    )

    trainer = SFTTrainer(
        model=model,
        args=training_args,
        train_dataset=dataset["train"],
        eval_dataset=dataset["eval"],
        processing_class=tokenizer,
    )

    print("Starting training...")
    train_result = trainer.train()

    print(f"\nTraining complete!")
    print(f"  Final loss: {train_result.training_loss:.4f}")
    print(f"  Total steps: {train_result.global_step}")

    # Save final model + tokenizer
    print(f"Saving model to {output_dir}...")
    trainer.save_model(output_dir)
    tokenizer.save_pretrained(output_dir)

    # Save training metrics
    metrics_path = os.path.join(output_dir, "train_metrics.json")
    with open(metrics_path, "w") as f:
        json.dump(train_result.metrics, f, indent=2)
    print(f"  Metrics saved to {metrics_path}")

    # Run final eval
    print("Running final evaluation...")
    eval_metrics = trainer.evaluate()
    print(f"  Eval loss: {eval_metrics['eval_loss']:.4f}")

    eval_metrics_path = os.path.join(output_dir, "eval_metrics.json")
    with open(eval_metrics_path, "w") as f:
        json.dump(eval_metrics, f, indent=2)
    print(f"  Eval metrics saved to {eval_metrics_path}")

    if use_wandb:
        wandb.finish()


if __name__ == "__main__":
    main()
