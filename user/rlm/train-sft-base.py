#!/usr/bin/env python3
"""SFT fine-tune Qwen3.5-4B-Base for Scheme fluency (completion mode).

Supports two modes:
  1. Single run:   python3 train-sft-base.py --model MODEL --lr 2e-5 ...
  2. Optuna sweep: python3 train-sft-base.py --optuna --study-db PATH --n-trials 10

Uses LoRA via PEFT. Merges LoRA into base weights at the end for vLLM serving.
All data is raw text (no chat template) — this is a base model.

Dependencies (pip install): trl peft transformers accelerate datasets optuna wandb
"""

import argparse
import json
import os
import sys
from pathlib import Path

import torch
from datasets import load_dataset
from peft import LoraConfig, get_peft_model, TaskType
from transformers import AutoTokenizer, AutoModelForCausalLM
from trl import SFTTrainer, SFTConfig


# --- Defaults ---
SCRIPT_DIR = Path(__file__).parent
DEFAULT_MODEL = str(Path.home() / "models" / "Qwen3.5-4B-Base-TextOnly")
DEFAULT_DATA_DIR = str(SCRIPT_DIR / "training" / "base")
DEFAULT_OUTPUT_DIR = str(SCRIPT_DIR / "models" / "qwen35-4b-base-sft")
DEFAULT_LR = 2e-5
DEFAULT_EPOCHS = 3
DEFAULT_BATCH_SIZE = 4
DEFAULT_GRAD_ACCUM = 4  # effective batch = 16
DEFAULT_MAX_SEQ_LEN = 2048
DEFAULT_WARMUP_RATIO = 0.10
DEFAULT_WEIGHT_DECAY = 0.01
DEFAULT_LORA_RANK = 16
DEFAULT_LORA_ALPHA = 16
LORA_TARGET_MODULES = [
    # Standard attention layers
    "q_proj", "k_proj", "v_proj", "o_proj",
    # DeltaNet layers (linear_attn)
    "in_proj_a", "in_proj_b", "in_proj_qkv", "in_proj_z",
    "out_proj",
    # MLP (shared across both layer types)
    "gate_proj", "up_proj", "down_proj",
]
WANDB_PROJECT = "fold-fluency-sft"


def parse_args():
    p = argparse.ArgumentParser(description="Completion-mode SFT for Qwen3.5-4B-Base")
    # Model & data
    p.add_argument("--model", default=DEFAULT_MODEL, help="Base model path")
    p.add_argument("--data-dir", default=DEFAULT_DATA_DIR, help="Directory with train.jsonl + eval.jsonl")
    p.add_argument("--output-dir", default=DEFAULT_OUTPUT_DIR, help="Output directory")
    # Training hyperparameters
    p.add_argument("--lr", type=float, default=DEFAULT_LR)
    p.add_argument("--epochs", type=int, default=DEFAULT_EPOCHS)
    p.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    p.add_argument("--grad-accum", type=int, default=DEFAULT_GRAD_ACCUM)
    p.add_argument("--max-seq-len", type=int, default=DEFAULT_MAX_SEQ_LEN)
    p.add_argument("--max-steps", type=int, default=-1, help="Max training steps (-1 = full epochs)")
    p.add_argument("--warmup-ratio", type=float, default=DEFAULT_WARMUP_RATIO)
    p.add_argument("--weight-decay", type=float, default=DEFAULT_WEIGHT_DECAY)
    # LoRA
    p.add_argument("--lora-rank", type=int, default=DEFAULT_LORA_RANK)
    p.add_argument("--lora-alpha", type=int, default=DEFAULT_LORA_ALPHA)
    p.add_argument("--no-lora", action="store_true", help="Full fine-tune (no LoRA)")
    # Optuna
    p.add_argument("--optuna", action="store_true", help="Run Optuna hyperparameter search")
    p.add_argument("--study-db", default=None, help="SQLite path for shared Optuna study")
    p.add_argument("--study-name", default="fluency-sft", help="Optuna study name")
    p.add_argument("--n-trials", type=int, default=10, help="Number of Optuna trials")
    # Misc
    p.add_argument("--run-name", default=None, help="W&B run name")
    p.add_argument("--no-wandb", action="store_true", help="Disable W&B logging")
    p.add_argument("--no-merge", action="store_true", help="Skip LoRA merge at end (save adapter only)")
    return p.parse_args()


def model_slug(model_id: str) -> str:
    return Path(model_id).name.lower()


def load_data(data_dir: str):
    """Load train/eval JSONL files."""
    train_path = os.path.join(data_dir, "train.jsonl")
    eval_path = os.path.join(data_dir, "eval.jsonl")

    if not os.path.exists(train_path):
        print(f"Error: {train_path} not found. Run prepare-sft-base.py first.", file=sys.stderr)
        sys.exit(1)

    data_files = {"train": train_path}
    if os.path.exists(eval_path):
        data_files["eval"] = eval_path

    return load_dataset("json", data_files=data_files)


def train_single(args, trial=None):
    """Run a single training job. If trial is provided, report to Optuna."""

    # Resolve hyperparameters (from Optuna trial or CLI args)
    if trial is not None:
        lr = trial.suggest_float("lr", 5e-6, 1e-4, log=True)
        if not args.no_lora:
            lora_rank = trial.suggest_categorical("lora_rank", [8, 16, 32])
            lora_alpha_mult = trial.suggest_categorical("lora_alpha_mult", [1, 2])
            lora_alpha = lora_rank * lora_alpha_mult
            tag = f"lora-r{lora_rank}-a{lora_alpha}"
        else:
            lora_rank = lora_alpha = None
            tag = "full"
        grad_accum = trial.suggest_categorical("grad_accum", [2, 4, 8])
        warmup_ratio = trial.suggest_float("warmup_ratio", 0.03, 0.15)
        weight_decay = trial.suggest_float("weight_decay", 0.0, 0.1)
        args.grad_accum = grad_accum  # override CLI default
        eff_bs = args.batch_size * grad_accum
        run_name = f"trial-{trial.number}-lr{lr:.1e}-bs{eff_bs}-{tag}"
    else:
        lr = args.lr
        lora_rank = args.lora_rank
        lora_alpha = args.lora_alpha
        warmup_ratio = args.warmup_ratio
        weight_decay = args.weight_decay
        run_name = args.run_name or f"{model_slug(args.model)}-sft-lr{lr}-r{lora_rank}"

    output_dir = os.path.join(args.output_dir, run_name) if trial else args.output_dir

    # W&B
    use_wandb = not args.no_wandb
    if use_wandb:
        try:
            import wandb
            wandb.init(
                project=WANDB_PROJECT,
                name=run_name,
                config={
                    "base_model": args.model,
                    "lr": lr,
                    "epochs": args.epochs,
                    "batch_size": args.batch_size,
                    "grad_accum": args.grad_accum,
                    "effective_batch_size": args.batch_size * args.grad_accum,
                    "max_seq_len": args.max_seq_len,
                    "warmup_ratio": warmup_ratio,
                    "weight_decay": weight_decay,
                    "lora_rank": lora_rank,
                    "lora_alpha": lora_alpha,
                    "lora": not args.no_lora,
                    "trial": trial.number if trial else None,
                },
                reinit=True,
            )
        except ImportError:
            print("wandb not installed, disabling", file=sys.stderr)
            use_wandb = False

    # Load tokenizer
    print(f"Loading tokenizer from {args.model}...")
    tokenizer = AutoTokenizer.from_pretrained(args.model, trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    # Load model
    print(f"Loading model from {args.model}...")
    model = AutoModelForCausalLM.from_pretrained(
        args.model,
        dtype=torch.bfloat16,
        trust_remote_code=True,
    )

    # Apply LoRA
    if not args.no_lora:
        print(f"Applying LoRA (rank={lora_rank}, alpha={lora_alpha})...")
        lora_config = LoraConfig(
            task_type=TaskType.CAUSAL_LM,
            r=lora_rank,
            lora_alpha=lora_alpha,
            target_modules=LORA_TARGET_MODULES,
            lora_dropout=0.0,
            bias="none",
        )
        model = get_peft_model(model, lora_config)
        model.print_trainable_parameters()

    # Load data
    print("Loading datasets...")
    dataset = load_data(args.data_dir)
    print(f"  train: {len(dataset['train'])}", end="")
    if "eval" in dataset:
        print(f", eval: {len(dataset['eval'])}")
    else:
        print()

    # Training config
    is_sweep = trial is not None
    training_args = SFTConfig(
        output_dir=output_dir,
        run_name=run_name,
        num_train_epochs=args.epochs,
        max_steps=args.max_steps,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        gradient_accumulation_steps=args.grad_accum,
        learning_rate=lr,
        warmup_ratio=warmup_ratio,
        weight_decay=weight_decay,
        bf16=True,
        logging_steps=10,
        eval_strategy="steps" if "eval" in dataset else "no",
        eval_steps=50,
        save_strategy="no" if is_sweep else "steps",
        save_steps=200,
        save_total_limit=2,
        max_length=args.max_seq_len,
        packing=True,
        dataset_text_field="text",
        report_to="wandb" if use_wandb else "none",
        dataloader_num_workers=2,
        gradient_checkpointing=True,
    )

    # Optuna pruning callback — report eval_loss at each eval step
    callbacks = []
    if trial is not None:
        import optuna
        from transformers import TrainerCallback

        class _OptunaReport(TrainerCallback):
            def on_evaluate(self, args, state, control, metrics=None, **kwargs):
                if metrics and "eval_loss" in metrics:
                    trial.report(metrics["eval_loss"], step=state.global_step)
                    if trial.should_prune():
                        raise optuna.TrialPruned()

        callbacks.append(_OptunaReport())

    trainer = SFTTrainer(
        model=model,
        args=training_args,
        train_dataset=dataset["train"],
        eval_dataset=dataset.get("eval"),
        processing_class=tokenizer,
        callbacks=callbacks,
    )

    # Train
    print("Starting training...")
    try:
        train_result = trainer.train()
    except Exception as e:
        # Clean up W&B on pruned trials or errors
        if use_wandb:
            import wandb
            wandb.finish(quiet=True)
        raise  # Optuna catches TrialPruned; other exceptions propagate

    print(f"\nTraining complete!")
    print(f"  Final loss: {train_result.training_loss:.4f}")
    print(f"  Total steps: {train_result.global_step}")

    # Eval
    eval_loss = None
    if "eval" in dataset:
        print("Running final evaluation...")
        eval_metrics = trainer.evaluate()
        eval_loss = eval_metrics.get("eval_loss")
        print(f"  Eval loss: {eval_loss:.4f}")

        if trial is None:
            metrics_path = os.path.join(output_dir, "eval_metrics.json")
            os.makedirs(output_dir, exist_ok=True)
            with open(metrics_path, "w") as f:
                json.dump(eval_metrics, f, indent=2)

    # Save (only for final single runs, not sweep trials)
    if trial is None:
        if not args.no_lora and not args.no_merge:
            print("Merging LoRA into base weights...")
            merged_model = model.merge_and_unload()
            print(f"Saving merged model to {output_dir}...")
            merged_model.save_pretrained(output_dir)
        else:
            print(f"Saving model to {output_dir}...")
            trainer.save_model(output_dir)
        tokenizer.save_pretrained(output_dir)

        metrics_path = os.path.join(output_dir, "train_metrics.json")
        with open(metrics_path, "w") as f:
            json.dump(train_result.metrics, f, indent=2)

    if use_wandb:
        import wandb
        wandb.finish()

    return eval_loss if eval_loss is not None else train_result.training_loss


def run_optuna(args):
    """Run Optuna hyperparameter search."""
    import optuna
    from optuna.pruners import MedianPruner
    from optuna.samplers import TPESampler

    storage = f"sqlite:///{args.study_db}" if args.study_db else None

    study = optuna.create_study(
        study_name=args.study_name,
        storage=storage,
        sampler=TPESampler(),
        pruner=MedianPruner(n_startup_trials=3, n_warmup_steps=50),
        direction="minimize",
        load_if_exists=True,
    )

    print(f"Optuna study '{args.study_name}' ({len(study.trials)} existing trials)")
    if storage:
        print(f"  Storage: {storage}")
    print(f"  Running {args.n_trials} new trials...")

    def objective(trial):
        return train_single(args, trial=trial)

    study.optimize(objective, n_trials=args.n_trials)

    # Report best
    print(f"\n{'='*60}")
    print(f"Best trial: #{study.best_trial.number}")
    print(f"  Value (loss): {study.best_trial.value:.4f}")
    print(f"  Params:")
    for key, value in study.best_trial.params.items():
        print(f"    {key}: {value}")
    print(f"\nTo run final training with best params:")
    params = study.best_trial.params
    alpha = params['lora_rank'] * params['lora_alpha_mult']
    print(f"  python3 {sys.argv[0]} \\")
    print(f"    --lr {params['lr']:.6e} \\")
    print(f"    --lora-rank {params['lora_rank']} \\")
    print(f"    --lora-alpha {alpha} \\")
    print(f"    --warmup-ratio {params['warmup_ratio']:.4f} \\")
    print(f"    --weight-decay {params['weight_decay']:.4f}")


def main():
    args = parse_args()

    if args.optuna:
        run_optuna(args)
    else:
        train_single(args)


if __name__ == "__main__":
    main()
