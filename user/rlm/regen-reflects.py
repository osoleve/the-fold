#!/usr/bin/env python3
"""Regenerate discovery trace reflections using multiple LLMs.

Reads discovery-traces.jsonl, replaces template reflect messages with
model-generated ones, writes discovery-traces-regen.jsonl.

8 models across 3 backends (gemini CLI, opencode CLI, vLLM HTTP).
"""

import json
import os
import re
import sys
import time
import subprocess
import requests
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

INPUT = Path("user/rlm/training/discovery-traces.jsonl")
OUTPUT = Path("user/rlm/training/discovery-traces-regen.jsonl")

SYSTEM = (
    "You are a concise technical note-taker. "
    "Respond with ONLY the note — one sentence, max 120 characters. "
    "Capture what was LEARNED or ACCOMPLISHED, not what action was taken. "
    "No preamble, no quotes, no 'Note:' prefix."
)

MODELS = [
    {"name": "gemini-3-pro",           "backend": "gemini",   "id": "gemini-3-pro-preview"},
    {"name": "gemini-3-flash",         "backend": "gemini",   "id": "gemini-3-flash-preview"},
    {"name": "opencode-trinity-large", "backend": "opencode", "id": "opencode/trinity-large-preview-free"},
    {"name": "opencode-gpt5-nano",     "backend": "opencode", "id": "opencode/gpt-5-nano"},
    {"name": "opencode-kimi-k2.5",     "backend": "opencode", "id": "opencode/kimi-k2.5-free"},
    {"name": "qwen3-next-local",       "backend": "vllm",     "id": "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8"},
]

ANSI_RE = re.compile(r'\x1b\[[0-9;]*[a-zA-Z]')


# ── Model backends ──────────────────────────────────────────────

def call_gemini(prompt, model_id):
    """Use gemini CLI in headless mode."""
    full_prompt = f"{SYSTEM}\n\n{prompt}"
    result = subprocess.run(
        ["gemini", "-m", model_id, full_prompt],
        capture_output=True, text=True, timeout=30,
    )
    if result.returncode != 0:
        raise RuntimeError(f"gemini error: {result.stderr[:200]}")
    return result.stdout.strip()


def call_opencode(prompt, model_id):
    """Use opencode CLI in headless mode."""
    full_prompt = f"{SYSTEM}\n\n{prompt}"
    result = subprocess.run(
        ["opencode", "run", "-m", model_id, full_prompt],
        capture_output=True, text=True, timeout=60,
    )
    out = ANSI_RE.sub('', result.stdout).strip()
    # opencode prepends "> build · model-name\n" — strip it
    lines = out.split('\n')
    content_lines = [l for l in lines if not l.strip().startswith('> build')]
    return '\n'.join(content_lines).strip()


def call_vllm(prompt, model_id):
    """Direct HTTP to local vLLM."""
    resp = requests.post(
        "http://localhost:8000/v1/chat/completions",
        json={
            "model": model_id,
            "max_tokens": 150,
            "temperature": 0.7,
            "messages": [
                {"role": "system", "content": SYSTEM},
                {"role": "user", "content": prompt},
            ],
            "chat_template_kwargs": {"enable_thinking": False},
        },
        timeout=60,
    )
    resp.raise_for_status()
    return resp.json()["choices"][0]["message"]["content"].strip()


BACKENDS = {
    "gemini": call_gemini,
    "opencode": call_opencode,
    "vllm": call_vllm,
}


# ── Postprocessing ──────────────────────────────────────────────

def clean_note(text):
    """Strip common LLM artifacts from reflection notes."""
    text = text.strip()
    # Remove wrapping quotes
    if len(text) > 2 and text[0] == '"' and text[-1] == '"':
        text = text[1:-1]
    if len(text) > 2 and text[0] == "'" and text[-1] == "'":
        text = text[1:-1]
    # Remove "Note:" prefix
    for prefix in ["Note:", "note:", "Note: ", "note: ", "Note -", "Note—"]:
        if text.startswith(prefix):
            text = text[len(prefix):].strip()
    # Remove backtick wrapping
    if text.startswith('`') and text.endswith('`'):
        text = text[1:-1]
    # Take first sentence if multi-sentence
    if '. ' in text and len(text) > 140:
        text = text[:text.index('. ') + 1]
    # Hard truncate
    if len(text) > 140:
        text = text[:137] + "..."
    return text.strip()


# ── Batch processing ────────────────────────────────────────────

def process_batch(batch, model_info):
    """Process a batch of (line_index, entry) pairs with one model."""
    name = model_info["name"]
    backend = model_info["backend"]
    model_id = model_info["id"]
    call_fn = BACKENDS[backend]

    results = []
    failures = 0
    for i, (line_idx, entry) in enumerate(batch):
        prompt = entry["messages"][0]["content"]
        try:
            raw_note = call_fn(prompt, model_id)
            note = clean_note(raw_note)
            if not note or len(note) < 5:
                raise ValueError(f"empty/tiny response: '{raw_note[:50]}'")
            results.append((line_idx, note, name))
        except Exception as e:
            failures += 1
            err = str(e)[:80]
            print(f"  [{name}] FAIL #{failures} idx={line_idx}: {err}", file=sys.stderr)
            original = entry["messages"][-1]["content"]
            results.append((line_idx, original, name + "/fallback"))

        if (i + 1) % 10 == 0:
            print(f"  [{name}] {i+1}/{len(batch)} done ({failures} failures)", file=sys.stderr)

        # Rate limiting
        if backend == "gemini":
            time.sleep(1.0)
        elif backend == "opencode":
            time.sleep(0.5)
        # vllm: no delay needed

    print(f"  [{name}] COMPLETE: {len(batch)} processed, {failures} failures", file=sys.stderr)
    return results


# ── Main ────────────────────────────────────────────────────────

def main():
    with open(INPUT) as f:
        all_lines = f.readlines()
    all_entries = [json.loads(line) for line in all_lines]
    print(f"Loaded {len(all_entries)} entries from {INPUT}", file=sys.stderr)

    reflects = [(i, entry) for i, entry in enumerate(all_entries)
                if entry.get("phase") == "reflect"]
    print(f"Found {len(reflects)} reflects to regenerate across {len(MODELS)} models", file=sys.stderr)

    # Distribute evenly
    batch_size = len(reflects) // len(MODELS)
    batches = []
    for j, model in enumerate(MODELS):
        start = j * batch_size
        end = start + batch_size if j < len(MODELS) - 1 else len(reflects)
        batches.append((reflects[start:end], model))
        print(f"  {model['name']:30s} → {end - start} reflects (idx {start}-{end-1})", file=sys.stderr)

    # Run all batches in parallel
    all_results = {}
    print(f"\nStarting parallel generation...", file=sys.stderr)
    t0 = time.time()

    with ThreadPoolExecutor(max_workers=len(MODELS)) as pool:
        futures = {
            pool.submit(process_batch, batch, model): model["name"]
            for batch, model in batches
        }
        for future in as_completed(futures):
            model_name = futures[future]
            try:
                results = future.result()
                for line_idx, note, source in results:
                    all_results[line_idx] = (note, source)
            except Exception as e:
                print(f"  [{model_name}] BATCH FAILED: {e}", file=sys.stderr)

    elapsed = time.time() - t0
    print(f"\nGeneration complete in {elapsed:.1f}s", file=sys.stderr)

    # Apply results
    updated = 0
    for line_idx, (note, source) in all_results.items():
        entry = all_entries[line_idx]
        entry["messages"][-1]["content"] = note
        entry["reflect_model"] = source
        updated += 1

    with open(OUTPUT, "w") as f:
        for entry in all_entries:
            f.write(json.dumps(entry, ensure_ascii=False) + "\n")

    print(f"Wrote {len(all_entries)} entries ({updated} reflects updated) to {OUTPUT}", file=sys.stderr)

    # Summary
    model_counts = {}
    fallback_counts = {}
    for _, source in all_results.values():
        if source.endswith("/fallback"):
            base = source.replace("/fallback", "")
            fallback_counts[base] = fallback_counts.get(base, 0) + 1
        else:
            model_counts[source] = model_counts.get(source, 0) + 1

    print("\n=== Results by model ===", file=sys.stderr)
    for model in sorted(set(list(model_counts.keys()) + list(fallback_counts.keys()))):
        ok = model_counts.get(model, 0)
        fail = fallback_counts.get(model, 0)
        print(f"  {model:30s}  ok={ok:3d}  fallback={fail}", file=sys.stderr)

    # Sample some results
    print("\n=== Sample regenerated reflects ===", file=sys.stderr)
    import random
    random.seed(42)
    regen_indices = [idx for idx, (_, src) in all_results.items() if not src.endswith("/fallback")]
    for idx in random.sample(regen_indices, min(10, len(regen_indices))):
        note, source = all_results[idx]
        print(f"  [{source:25s}] {note}", file=sys.stderr)


if __name__ == "__main__":
    main()
