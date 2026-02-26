import torch
import argparse
from datasets import load_dataset
from trl import SFTConfig, SFTTrainer
from transformers import AutoModelForCausalLM, AutoTokenizer


def main(args):
    print(f"Loading model: {args.model_name}")
    model = AutoModelForCausalLM.from_pretrained(
        args.model_name,
        dtype=args.dtype,
        device_map="auto",
        trust_remote_code=True,
    )
    tokenizer = AutoTokenizer.from_pretrained(args.model_name, trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    total_params = sum(p.numel() for p in model.parameters())
    trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
    print(f"Total parameters: {total_params:,}")
    print(f"Trainable parameters: {trainable_params:,} ({100*trainable_params/total_params:.1f}%)")

    # Load chat JSONL datasets (OpenAI messages format)
    print(f"Loading training data from {args.train_data}")
    train_dataset = load_dataset("json", data_files=args.train_data, split="train")
    print(f"  Train examples: {len(train_dataset)}")

    eval_dataset = None
    if args.eval_data:
        eval_dataset = load_dataset("json", data_files=args.eval_data, split="train")
        print(f"  Eval examples: {len(eval_dataset)}")

    config = SFTConfig(
        per_device_train_batch_size=args.batch_size,
        gradient_accumulation_steps=args.gradient_accumulation_steps,
        num_train_epochs=args.num_epochs,
        learning_rate=args.learning_rate,
        optim="adamw_torch",
        lr_scheduler_type="cosine",
        warmup_ratio=0.1,
        weight_decay=0.01,
        max_length=args.seq_length,
        packing=False,
        gradient_checkpointing=args.gradient_checkpointing,
        save_strategy="steps",
        save_steps=args.save_steps,
        eval_strategy="steps" if eval_dataset else "no",
        eval_steps=args.eval_steps if eval_dataset else None,
        logging_steps=args.logging_steps,
        logging_dir=args.log_dir,
        output_dir=args.output_dir or "/tmp/rlm-sft-output",
        report_to="wandb" if not args.no_wandb else "none",
        run_name=args.run_name,
        seed=42,
        bf16=True,
        remove_unused_columns=False,
    )

    trainer = SFTTrainer(
        model=model,
        processing_class=tokenizer,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        args=config,
    )

    print(f"\nStarting SFT for {args.num_epochs} epoch(s)...")
    trainer_stats = trainer.train()

    print(f"\n{'='*60}")
    print("TRAINING COMPLETED")
    print(f"{'='*60}")
    print(f"Training runtime: {trainer_stats.metrics['train_runtime']:.2f} seconds")
    print(f"Samples per second: {trainer_stats.metrics['train_samples_per_second']:.2f}")
    print(f"Steps per second: {trainer_stats.metrics['train_steps_per_second']:.2f}")
    print(f"Train loss: {trainer_stats.metrics['train_loss']:.4f}")
    print(f"{'='*60}\n")

    if args.output_dir:
        print(f"Saving final model to {args.output_dir}...")
        trainer.save_model(args.output_dir)
        tokenizer.save_pretrained(args.output_dir)
        print("Model saved successfully!")


def parse_arguments():
    parser = argparse.ArgumentParser(description="RLM v2 SFT Training")

    # Model
    parser.add_argument("--model_name", type=str, default="meta-llama/Llama-3.2-3B-Instruct")
    parser.add_argument("--dtype", type=str, default="bfloat16",
                        choices=["float32", "float16", "bfloat16"])

    # Data
    parser.add_argument("--train_data", type=str, default="/workspace/training/train.jsonl")
    parser.add_argument("--eval_data", type=str, default="/workspace/training/eval.jsonl")

    # Training
    parser.add_argument("--batch_size", type=int, default=4)
    parser.add_argument("--gradient_accumulation_steps", type=int, default=4)
    parser.add_argument("--seq_length", type=int, default=2048)
    parser.add_argument("--num_epochs", type=int, default=3)
    parser.add_argument("--learning_rate", type=float, default=2e-5)
    parser.add_argument("--gradient_checkpointing", action="store_true")

    # Checkpointing
    parser.add_argument("--save_steps", type=int, default=200)
    parser.add_argument("--eval_steps", type=int, default=50)

    # Logging
    parser.add_argument("--logging_steps", type=int, default=5)
    parser.add_argument("--log_dir", type=str, default="logs")
    parser.add_argument("--run_name", type=str, default="llama3-3b-rlm-sft")
    parser.add_argument("--no_wandb", action="store_true")

    # Output
    parser.add_argument("--output_dir", type=str,
                        default="/workspace/models/llama3-3b-rlm-sft")

    return parser.parse_args()


if __name__ == "__main__":
    args = parse_arguments()
    print(f"\n{'='*60}")
    print("RLM v2 SFT TRAINING")
    print(f"{'='*60}")
    print(f"Model: {args.model_name}")
    print(f"Train data: {args.train_data}")
    print(f"Eval data: {args.eval_data}")
    print(f"Batch size: {args.batch_size} x {args.gradient_accumulation_steps} = {args.batch_size * args.gradient_accumulation_steps}")
    print(f"Seq length: {args.seq_length}")
    print(f"Epochs: {args.num_epochs}")
    print(f"LR: {args.learning_rate}")
    print(f"Output: {args.output_dir}")
    print(f"{'='*60}\n")

    main(args)
