#!/usr/bin/env python3
"""Download samples from GSM8K and DROP, save as sexp files."""

import os
import random
import re

os.environ["HF_HOME"] = "/tmp/hf_cache"

from datasets import load_dataset

SEED = 42
N = 100


def escape_sexp_string(s: str) -> str:
    """Escape a string for valid Scheme read syntax."""
    s = s.replace("\\", "\\\\")   # backslashes first
    s = s.replace('"', '\\"')     # double quotes
    s = s.replace("\n", "\\n")    # newlines
    s = s.replace("\r", "")       # carriage returns
    s = s.replace("\t", " ")      # tabs to spaces
    return s


def extract_gsm8k_answer(answer_text: str):
    """Extract the final numeric answer after ####."""
    m = re.search(r"####\s*(-?[\d,]+)", answer_text)
    if m:
        return int(m.group(1).replace(",", ""))
    return None


def count_steps(answer_text: str) -> int:
    """Rough heuristic: count non-empty, non-#### lines."""
    lines = [l.strip() for l in answer_text.split("\n") if l.strip() and not l.strip().startswith("####")]
    return len(lines)


# -- GSM8K ---------------------------------------------------------------

print("Loading GSM8K test split...")
gsm8k = load_dataset("openai/gsm8k", "main", split="test")

rng = random.Random(SEED)
candidates = []
for item in gsm8k:
    ans = extract_gsm8k_answer(item["answer"])
    if ans is not None:
        steps = count_steps(item["answer"])
        candidates.append({
            "question": item["question"],
            "answer": ans,
            "steps": steps,
        })

candidates.sort(key=lambda x: x["steps"])

easy = [c for c in candidates if c["steps"] <= 2]
medium = [c for c in candidates if 3 <= c["steps"] <= 4]
hard = [c for c in candidates if c["steps"] >= 5]

print(f"  Candidates: {len(candidates)} (easy={len(easy)}, medium={len(medium)}, hard={len(hard)})")

rng.shuffle(easy)
rng.shuffle(medium)
rng.shuffle(hard)

selected_gsm8k = easy[:25] + medium[:40] + hard[:35]
rng.shuffle(selected_gsm8k)

with open("/home/osoleve/fold/user/rlm/data/gsm8k-sample.sexp", "w") as f:
    f.write("((gsm8k-sample\n")
    for i, item in enumerate(selected_gsm8k):
        q = escape_sexp_string(item["question"])
        f.write(f'  ((question . "{q}")\n')
        f.write(f'   (answer . {item["answer"]}))')
        if i < len(selected_gsm8k) - 1:
            f.write("\n")
    f.write("))\n")

print(f"  Wrote {len(selected_gsm8k)} GSM8K samples")


# -- DROP -----------------------------------------------------------------

print("Loading DROP validation split...")
drop = load_dataset("ucinlp/drop", split="validation")

candidates_drop = []
for item in drop:
    spans = item["answers_spans"]["spans"]
    types = item["answers_spans"]["types"]

    if not spans or not types:
        continue

    answer = spans[0]
    answer_type = types[0]

    is_numeric = False
    if answer_type == "number":
        is_numeric = True
    elif re.match(r"^-?\d+(\.\d+)?$", answer.strip()):
        is_numeric = True

    if is_numeric and answer.strip():
        passage = item["passage"]
        if len(passage) < 3000:
            candidates_drop.append({
                "passage": passage,
                "question": item["question"],
                "answer": answer.strip(),
                "answer_type": "number",
            })

print(f"  Numeric answer candidates: {len(candidates_drop)}")

rng.shuffle(candidates_drop)
selected_drop = candidates_drop[:N]

with open("/home/osoleve/fold/user/rlm/data/drop-sample.sexp", "w") as f:
    f.write("((drop-sample\n")
    for i, item in enumerate(selected_drop):
        p = escape_sexp_string(item["passage"])
        q = escape_sexp_string(item["question"])
        a = escape_sexp_string(item["answer"])
        f.write(f'  ((passage . "{p}")\n')
        f.write(f'   (question . "{q}")\n')
        f.write(f'   (answer . "{a}")\n')
        f.write(f'   (answer-type . "number"))')
        if i < len(selected_drop) - 1:
            f.write("\n")
    f.write("))\n")

print(f"  Wrote {len(selected_drop)} DROP samples")


# -- Verify ---------------------------------------------------------------

print("\n== First GSM8K entry ==")
with open("/home/osoleve/fold/user/rlm/data/gsm8k-sample.sexp") as f:
    content = f.read()
    start = content.index("((question")
    depth = 0
    for i, c in enumerate(content[start:], start):
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                print(content[start:i+1])
                break

print("\n== First DROP entry ==")
with open("/home/osoleve/fold/user/rlm/data/drop-sample.sexp") as f:
    content = f.read()
    start = content.index("((passage")
    depth = 0
    for i, c in enumerate(content[start:], start):
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                print(content[start:i+1])
                break

print("\nDone.")
