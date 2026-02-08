#!/usr/bin/env python3
"""Merge SFT training data from multiple sources and split train/eval.

Sources:
  - discovery-traces.jsonl (1326 examples, multi-model reflect regen)
  - collected-traces.jsonl (887 examples, benchmark trajectories)
  - sft-training-data.jsonl (397 examples, hand-crafted synthetic)

Output:
  - train.jsonl (95% stratified by source)
  - eval.jsonl (5% stratified by source)
"""

import json
import random
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PARENT_DIR = os.path.dirname(SCRIPT_DIR)

SOURCES = [
    (os.path.join(SCRIPT_DIR, "discovery-traces.jsonl"), "discovery"),
    (os.path.join(SCRIPT_DIR, "collected-traces.jsonl"), "collected"),
    (os.path.join(PARENT_DIR, "sft-training-data.jsonl"), "synthetic"),
]

EVAL_FRACTION = 0.05
SEED = 42


def load_jsonl(path, tag):
    examples = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            obj = json.loads(line)
            # Keep only the messages field for TRL, plus source tag for splitting
            examples.append({
                "messages": obj["messages"],
                "_source": tag,
            })
    return examples


def main():
    random.seed(SEED)

    # Load all sources
    all_by_source = {}
    total = 0
    for path, tag in SOURCES:
        examples = load_jsonl(path, tag)
        all_by_source[tag] = examples
        print(f"  {tag}: {len(examples)} examples")
        total += len(examples)
    print(f"  total: {total}")

    # Stratified split
    train, eval_ = [], []
    for tag, examples in all_by_source.items():
        shuffled = list(examples)
        random.shuffle(shuffled)
        n_eval = max(1, int(len(shuffled) * EVAL_FRACTION))
        eval_.extend(shuffled[:n_eval])
        train.extend(shuffled[n_eval:])

    random.shuffle(train)
    random.shuffle(eval_)

    print(f"\n  train: {len(train)}, eval: {len(eval_)}")

    # Write output — strip _source tag, keep only messages
    def write_jsonl(path, data):
        with open(path, "w") as f:
            for item in data:
                out = {"messages": item["messages"]}
                f.write(json.dumps(out, ensure_ascii=False) + "\n")

    train_path = os.path.join(SCRIPT_DIR, "train.jsonl")
    eval_path = os.path.join(SCRIPT_DIR, "eval.jsonl")

    write_jsonl(train_path, train)
    write_jsonl(eval_path, eval_)

    print(f"\n  wrote {train_path}")
    print(f"  wrote {eval_path}")


if __name__ == "__main__":
    main()
