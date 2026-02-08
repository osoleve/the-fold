#!/usr/bin/env python3
"""Download samples from MATH (Hendrycks) and HotpotQA for RLM v2 benchmarks.

MATH: Competition-level math problems that require multi-step reasoning.
HotpotQA: Multi-hop QA requiring reasoning across multiple paragraphs.
"""

import os
import random
import re

os.environ["HF_HOME"] = "/tmp/hf_cache"

from datasets import load_dataset

SEED = 42
rng = random.Random(SEED)


def escape_sexp(s):
    """Escape a string for S-expression format."""
    s = s.replace("\\", "\\\\")
    s = s.replace('"', '\\"')
    s = s.replace("\n", "\\n")
    s = s.replace("\r", "")
    return s


def write_sexp(items, label, path, fields):
    """Write items as S-expression to file."""
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
    """Check if string is a clean numeric answer."""
    s = s.strip()
    # Remove \boxed{} wrapper if present
    m = re.match(r"\\boxed\{(.+)\}", s)
    if m:
        s = m.group(1).strip()
    # Remove dollar signs and commas
    s = s.replace("$", "").replace(",", "").strip()
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False


def extract_math_answer(solution):
    """Extract the final answer from a MATH solution string."""
    # Look for \boxed{...}
    m = re.search(r"\\boxed\{([^{}]+)\}", solution)
    if m:
        ans = m.group(1).strip()
        # Clean up common formatting
        ans = ans.replace("$", "").replace(",", "").strip()
        return ans
    return None


# --- MATH (Hendrycks) ---
print("Loading MATH-500 test split...")
math = load_dataset("HuggingFaceH4/MATH-500", split="test")
print(f"  Total: {len(math)}")

math_items = []
for row in math:
    answer = row.get("answer", "")
    # Use direct answer field; fall back to extraction from solution
    if not answer:
        solution = row.get("solution", "")
        answer = extract_math_answer(solution)
    if not answer or not is_clean_number(answer):
        continue

    question = row["problem"]
    level = str(row.get("level", "unknown"))
    subject = row.get("subject", "unknown")

    # Skip very long problems
    if len(question) > 3000:
        continue

    math_items.append({
        "question": question,
        "answer": answer,
        "level": level,
        "subject": subject,
    })

print(f"  Numeric-answer problems: {len(math_items)}")

# Stratify by level
by_level = {}
for item in math_items:
    by_level.setdefault(item["level"], []).append(item)

for level, items in sorted(by_level.items()):
    print(f"    {level}: {len(items)}")

# Sample ~50 problems across difficulty levels
sampled_math = []
for level, items in by_level.items():
    rng.shuffle(items)
    # Take proportional share, at least 3 per level
    n = max(3, min(len(items), 50 * len(items) // len(math_items) + 1))
    sampled_math.extend(items[:n])

if len(sampled_math) > 50:
    rng.shuffle(sampled_math)
    sampled_math = sampled_math[:50]

rng.shuffle(sampled_math)

write_sexp(sampled_math, "math-competition",
           "user/rlm/data/math-competition-sample.sexp",
           ["question", "answer", "level", "subject"])

# --- HotpotQA ---
print("\nLoading HotpotQA validation (distractor) split...")
hotpot = load_dataset("hotpot_qa", "distractor", split="validation")
print(f"  Total: {len(hotpot)}")

hotpot_items = []
for row in hotpot:
    answer = row.get("answer", "").strip()
    question = row.get("question", "")
    level = row.get("level", "")

    # Build context from supporting paragraphs
    titles = row.get("context", {}).get("title", [])
    sentences = row.get("context", {}).get("sentences", [])

    paragraphs = []
    for title, sents in zip(titles, sentences):
        para = " ".join(sents)
        paragraphs.append(f"[{title}] {para}")

    context = "\n\n".join(paragraphs)

    # Skip if context is too short (not useful) or too long
    if len(context) < 200 or len(context) > 8000:
        continue

    # Skip if answer is too long (we want concise answers for auto-eval)
    if len(answer) > 50:
        continue

    # Prefer numeric or short factual answers
    hotpot_items.append({
        "question": question,
        "answer": answer,
        "context": context,
        "level": level,
        "context_len": len(context),
    })

print(f"  Suitable items: {len(hotpot_items)}")

# Separate by whether answer is numeric
numeric_hotpot = [x for x in hotpot_items if is_clean_number(x["answer"])]
text_hotpot = [x for x in hotpot_items if not is_clean_number(x["answer"])]
print(f"    Numeric answers: {len(numeric_hotpot)}")
print(f"    Text answers: {len(text_hotpot)}")

# Prefer longer contexts (forces chunked processing)
# Sort by context length descending, take from longer end
text_hotpot.sort(key=lambda x: x["context_len"], reverse=True)
numeric_hotpot.sort(key=lambda x: x["context_len"], reverse=True)

# Sample: 20 numeric + 30 text (text exercises grep/search more)
rng.shuffle(numeric_hotpot[:200])  # Randomize among top 200 longest
rng.shuffle(text_hotpot[:500])
sampled_hotpot = numeric_hotpot[:20] + text_hotpot[:30]
rng.shuffle(sampled_hotpot)

# Remove context_len from output
for item in sampled_hotpot:
    del item["context_len"]

write_sexp(sampled_hotpot, "hotpotqa",
           "user/rlm/data/hotpotqa-sample.sexp",
           ["question", "answer", "context", "level"])

# --- Summary ---
print(f"\n=== Summary ===")
print(f"MATH Competition: {len(sampled_math)} problems")
print(f"HotpotQA:         {len(sampled_hotpot)} problems")
print(f"Total new:        {len(sampled_math) + len(sampled_hotpot)}")
