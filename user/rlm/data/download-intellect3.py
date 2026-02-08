#!/usr/bin/env python3
"""Download samples from PrimeIntellect/INTELLECT-3-RL for RLM v2 benchmarks.

Filters to problems with clean numeric answers suitable for `(submit N)`.
Extracts answers from nested JSON metadata where needed.
"""

import os
import json
import random
import re

os.environ["HF_HOME"] = "/tmp/hf_cache"

from datasets import load_dataset

random.seed(42)

def escape_sexp(s):
    """Escape a string for S-expression format."""
    s = s.replace("\\", "\\\\")
    s = s.replace('"', '\\"')
    s = s.replace("\n", "\\n")
    s = s.replace("\r", "")
    return s

def write_sexp(items, label, path, fields):
    """Write items as S-expression to file, using only specified fields."""
    with open(path, "w") as f:
        f.write(f"(({label}\n")
        for item in items:
            f.write("  (")
            first = True
            for k in fields:
                if k not in item:
                    continue
                if not first:
                    f.write("\n    ")
                first = False
                f.write(f'({k} . "{escape_sexp(str(item[k]))}")')
            f.write(")\n")
        f.write("))\n")
    print(f"  Wrote {len(items)} items to {path}")

def is_clean_number(s):
    """Check if string is a clean numeric answer (int, float, percentage)."""
    s = s.strip().rstrip("%")
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False

def clean_number(s):
    """Clean a numeric string."""
    s = s.strip()
    if s.endswith("%"):
        return s  # Keep percentage as-is for display
    try:
        n = float(s)
        if n == int(n):
            return str(int(n))
        return s
    except ValueError:
        return s

# --- Logic: Object Counting ---
print("Loading Logic subset...")
logic = load_dataset("PrimeIntellect/INTELLECT-3-RL", "logic", split="train")
print(f"  Total: {len(logic)}")

object_counting = []
mathador = []

for row in logic:
    difficulty = row.get("avg@16_qwen3_4b_instruct_2507", 0.5)
    info_str = row.get("info", "")

    try:
        info = json.loads(info_str)
    except (json.JSONDecodeError, TypeError):
        continue

    task_type = info.get("task", "")

    if task_type == "object_counting":
        # Answer is in nested game_data_str JSON
        try:
            game_data = json.loads(info.get("game_data_str", "{}"))
            answer = game_data.get("answer", "")
            if is_clean_number(answer):
                question = row["question"]
                # Truncate very long questions (some are 5K+ chars)
                if len(question) > 4000:
                    question = question[:4000] + "\n[truncated]"
                object_counting.append({
                    "question": question,
                    "answer": clean_number(answer),
                    "task": "object_counting",
                    "difficulty": round(difficulty, 3),
                })
        except (json.JSONDecodeError, TypeError):
            continue

    elif task_type == "mathador":
        try:
            game_data = json.loads(info.get("game_data_str", "{}"))
            metadata = game_data.get("metadata", {})
            result = metadata.get("result")
            numbers = metadata.get("numbers", [])
            if result is not None and numbers:
                mathador.append({
                    "question": row["question"],
                    "answer": str(result),
                    "numbers": str(numbers),
                    "task": "mathador",
                    "difficulty": round(difficulty, 3),
                })
        except (json.JSONDecodeError, TypeError):
            continue

print(f"  Object Counting with numeric answers: {len(object_counting)}")
print(f"  Mathador with targets: {len(mathador)}")

# Sample 10 Object Counting at medium difficulty
medium_oc = [x for x in object_counting if 0.1 <= x["difficulty"] <= 0.6]
print(f"  Medium OC (0.1-0.6): {len(medium_oc)}")
sampled_oc = random.sample(medium_oc, min(30, len(medium_oc)))

# Sample 10 Mathador
medium_math = [x for x in mathador if 0.05 <= x["difficulty"] <= 0.5]
print(f"  Medium Mathador (0.05-0.5): {len(medium_math)}")
sampled_mathador = random.sample(medium_math, min(30, len(medium_math)))

# Combine into logic sample
sampled_logic = sampled_oc + sampled_mathador
write_sexp(sampled_logic, "intellect3-logic",
           "user/rlm/data/intellect3-logic-sample.sexp",
           ["question", "answer", "task", "difficulty"])

# --- Science (numeric answers only) ---
print("\nLoading Science subset...")
science = load_dataset("PrimeIntellect/INTELLECT-3-RL", "science", split="train")
print(f"  Total: {len(science)}")

science_items = []
for row in science:
    difficulty = row.get("avg@16_qwen3_4b_instruct_2507", 0.5)
    answer = str(row.get("answer", "")).strip()
    question = row["question"]

    # Only keep clean numeric answers
    if not is_clean_number(answer):
        continue

    # Skip very long questions
    if len(question) > 3000:
        continue

    info = row.get("info", {})
    domain = info.get("domain", "unknown") if isinstance(info, dict) else "unknown"

    science_items.append({
        "question": question,
        "answer": clean_number(answer),
        "domain": domain,
        "difficulty": round(difficulty, 3),
    })

print(f"  Numeric-answer science: {len(science_items)}")

# Sample 10 at medium difficulty, diverse domains
medium_sci = [x for x in science_items if 0.05 <= x["difficulty"] <= 0.5]
by_domain = {}
for item in medium_sci:
    by_domain.setdefault(item["domain"], []).append(item)
print(f"  Medium (0.05-0.5): {len(medium_sci)} across {len(by_domain)} domains")

sampled_science = []
for domain, items in by_domain.items():
    n = max(2, min(5, len(items) * 10 // max(1, len(medium_sci))))
    sampled_science.extend(random.sample(items, min(n, len(items))))

if len(sampled_science) > 30:
    sampled_science = random.sample(sampled_science, 30)

write_sexp(sampled_science, "intellect3-science",
           "user/rlm/data/intellect3-science-sample.sexp",
           ["question", "answer", "domain", "difficulty"])

# --- Math (clean numeric answers only) ---
print("\nLoading Math subset...")
math = load_dataset("PrimeIntellect/INTELLECT-3-RL", "math", split="train")
print(f"  Total: {len(math)}")

math_items = []
for row in math:
    diff_inst = row.get("avg@8_qwen3_4b_instruct_2507", 0.5)
    answer = str(row.get("answer", "")).strip()
    question = row["question"]

    # Only keep clean numeric answers (skip LaTeX, fractions, symbolic)
    if not is_clean_number(answer):
        continue

    if len(question) > 3000:
        continue

    math_items.append({
        "question": question,
        "answer": clean_number(answer),
        "difficulty": round(diff_inst, 3),
    })

print(f"  Numeric-answer math: {len(math_items)}")

medium_math_q = [x for x in math_items if 0.05 <= x["difficulty"] <= 0.5]
print(f"  Medium (0.05-0.5): {len(medium_math_q)}")
sampled_math_q = random.sample(medium_math_q, min(30, len(medium_math_q)))

write_sexp(sampled_math_q, "intellect3-math",
           "user/rlm/data/intellect3-math-sample.sexp",
           ["question", "answer", "difficulty"])

# Summary
print(f"\n=== Summary ===")
print(f"Logic:   {len(sampled_logic)} problems ({len(sampled_oc)} obj counting + {len(sampled_mathador)} mathador)")
print(f"Science: {len(sampled_science)} problems (numeric only)")
print(f"Math:    {len(sampled_math_q)} problems (numeric only)")
print(f"Total:   {len(sampled_logic) + len(sampled_science) + len(sampled_math_q)} problems")
