#!/usr/bin/env python3
"""
Ensemble LLM review of commented-out code in The Fold's lattice layer.

Approach: Send full files to the model, ask it to emit dead code spans
in <del>...</del> tags. Run N times per file with temperature variation.
Spans that appear across multiple runs are high-confidence delete candidates.

Usage:
  python3 boundary/tools/comment-review.py scan        # List files that will be reviewed
  python3 boundary/tools/comment-review.py review      # Full pipeline
  python3 boundary/tools/comment-review.py review -n 3 # 3 runs per file (default 5)
"""

import re
import sys
import time
import concurrent.futures
import requests
from pathlib import Path
from collections import defaultdict

VLLM_URL = "http://localhost:8000/v1/chat/completions"
MODEL = "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8"
RUNS_PER_FILE = 5
TEMPERATURES = [0.3, 0.4, 0.5, 0.7, 0.9]
MAX_WORKERS = 5
LATTICE_ROOT = Path("lattice")


SYSTEM_PROMPT = """\
You review Scheme source files for commented-out dead code.

KEEP (do NOT emit):
- Doc comments describing functions, types, or modules
- Type signatures in comments (;;; name : Type -> Type -> Result)
- Usage examples near the definitions they document
- Mathematical derivations and formulas
- Format specifications and data schemas
- Section headers and decorative separators
- TODO/NOTE/FIXME/FIX/HACK/XXX annotations and BBS ticket references

DELETE (emit in <del> tags):
- Commented-out function definitions that have been replaced
- Debug scaffolding (display, printf, trace calls)
- Abandoned test expressions
- Old code kept "for reference" that version control already preserves
- Commented-out load/require statements for removed dependencies

For each block of dead code, wrap it exactly as it appears in the file:
<del>
;; the commented-out lines exactly as they appear
</del>

If the file has no dead code to delete, respond with just: CLEAN

Do not explain your reasoning. Only emit <del> blocks or CLEAN."""


def find_candidate_files(root):
    """Find .ss files that contain commented-out s-expressions."""
    files = []
    for ss_file in sorted(root.rglob("*.ss")):
        rel = str(ss_file.relative_to(Path(".")))
        if ".bak" in rel:
            continue
        try:
            text = ss_file.read_text()
        except Exception:
            continue

        # Quick check: does this file have any commented s-expressions?
        if re.search(r"^\s*;+\s*\(", text, re.MULTILINE):
            lines = text.splitlines()
            files.append({
                "path": rel,
                "content": text,
                "line_count": len(lines),
            })
    return files


def review_file(file_info, temperature, run_id):
    """Send a full file for review. Returns list of del-tagged spans."""
    try:
        r = requests.post(VLLM_URL, json={
            "model": MODEL,
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": (
                    f"File: {file_info['path']}\n\n"
                    f"{file_info['content']}\n\n"
                    f"Emit any commented-out dead code in <del>...</del> tags."
                )},
            ],
            "max_tokens": 4000,
            "temperature": temperature,
        }, timeout=120)
        text = r.json()["choices"][0]["message"]["content"].strip()
        usage = r.json().get("usage", {})
        return {
            "run_id": run_id,
            "temperature": temperature,
            "response": text,
            "prompt_tokens": usage.get("prompt_tokens", 0),
            "completion_tokens": usage.get("completion_tokens", 0),
        }
    except Exception as e:
        return {
            "run_id": run_id,
            "temperature": temperature,
            "response": f"ERROR: {e}",
            "prompt_tokens": 0,
            "completion_tokens": 0,
        }


def extract_del_blocks(response_text):
    """Parse <del>...</del> blocks from model response."""
    blocks = []
    for m in re.finditer(r"<del>\s*\n?(.*?)\n?\s*</del>", response_text, re.DOTALL):
        content = m.group(1).strip()
        if content:
            blocks.append(content)
    return blocks


def locate_span(file_content, del_block):
    """Find the line range of a del block within the source file.

    Returns (start_line, end_line) 1-indexed, or None if not found.
    Uses progressive fuzzy matching: first tries exact match,
    then strips whitespace, then matches on longest contiguous substring.
    """
    file_lines = file_content.splitlines()
    del_lines = del_block.strip().splitlines()
    if not del_lines:
        return None

    # Normalize for matching
    def norm(line):
        return line.strip()

    del_normed = [norm(l) for l in del_lines]

    # Slide a window over file lines looking for best match
    best_start = None
    best_score = 0

    for i in range(len(file_lines) - len(del_normed) + 1):
        matches = sum(
            1 for j, dl in enumerate(del_normed)
            if norm(file_lines[i + j]) == dl
        )
        score = matches / len(del_normed)
        if score > best_score:
            best_score = score
            best_start = i

    # Require at least 60% line match
    if best_score >= 0.6 and best_start is not None:
        span = (best_start + 1, best_start + len(del_normed))
    else:
        # Fallback: try matching first line only
        span = None
        first = del_normed[0]
        for i, fl in enumerate(file_lines):
            if norm(fl) == first:
                span = (i + 1, i + len(del_normed))
                break

    if span is None:
        return None

    # Guard: verify span lines are actually comments, not live code
    start_idx, end_idx = span[0] - 1, span[1]  # back to 0-indexed
    comment_lines = sum(
        1 for l in file_lines[start_idx:end_idx]
        if l.strip().startswith(";") or l.strip() == ""
    )
    if comment_lines / max(1, end_idx - start_idx) < 0.5:
        return None  # More than half non-comment = not commented-out code

    return span


def aggregate_spans(file_path, file_content, all_runs):
    """Aggregate spans across runs. Return spans with vote counts."""
    # Collect all identified spans across runs
    span_votes = defaultdict(lambda: {"count": 0, "runs": [], "content": ""})

    for run in all_runs:
        if "ERROR" in run["response"] or run["response"].strip() == "CLEAN":
            continue
        blocks = extract_del_blocks(run["response"])
        for block in blocks:
            loc = locate_span(file_content, block)
            if loc is None:
                continue
            key = loc  # (start, end) tuple
            span_votes[key]["count"] += 1
            span_votes[key]["runs"].append(run["run_id"])
            span_votes[key]["content"] = block

    # Merge overlapping spans (within ±2 lines)
    merged = {}
    for (start, end), info in sorted(span_votes.items()):
        found_merge = False
        for (ms, me) in list(merged.keys()):
            if abs(start - ms) <= 2 and abs(end - me) <= 2:
                # Merge into existing
                merged[(ms, me)]["count"] += info["count"]
                merged[(ms, me)]["runs"].extend(info["runs"])
                if len(info["content"]) > len(merged[(ms, me)]["content"]):
                    merged[(ms, me)]["content"] = info["content"]
                found_merge = True
                break
        if not found_merge:
            merged[(start, end)] = dict(info)

    return merged


def run_pipeline(n_runs=None):
    """Full pipeline: scan files, review, aggregate, report."""
    n_runs = n_runs or RUNS_PER_FILE

    print("Phase 1: Scanning for candidate files...")
    files = find_candidate_files(LATTICE_ROOT)
    total_lines = sum(f["line_count"] for f in files)
    print(f"  {len(files)} files, {total_lines} total lines")
    print()

    total_requests = len(files) * n_runs
    print(f"Phase 2: Reviewing ({n_runs} runs/file, {total_requests} requests, {MAX_WORKERS} concurrent)...")

    # Build all tasks
    tasks = []
    for file_idx, finfo in enumerate(files):
        for run_idx in range(n_runs):
            temp = TEMPERATURES[run_idx % len(TEMPERATURES)]
            tasks.append((file_idx, finfo, temp, f"{file_idx}-{run_idx}"))

    # Execute in parallel
    start_time = time.time()
    results_by_file = defaultdict(list)

    with concurrent.futures.ThreadPoolExecutor(max_workers=MAX_WORKERS) as ex:
        futures = {
            ex.submit(review_file, finfo, temp, rid): file_idx
            for file_idx, finfo, temp, rid in tasks
        }
        done = 0
        total_in = 0
        total_out = 0
        for future in concurrent.futures.as_completed(futures):
            file_idx = futures[future]
            result = future.result()
            results_by_file[file_idx].append(result)
            total_in += result["prompt_tokens"]
            total_out += result["completion_tokens"]
            done += 1
            if done % 10 == 0 or done == total_requests:
                elapsed = time.time() - start_time
                print(f"  {done}/{total_requests} ({elapsed:.1f}s, {done/elapsed:.1f} req/s)")

    elapsed = time.time() - start_time
    print(f"  Done: {total_requests} requests in {elapsed:.1f}s")
    print(f"  Tokens: {total_in:,} in, {total_out:,} out")
    print()

    # Phase 3: Aggregate
    print("Phase 3: Aggregating across runs...")
    all_candidates = []
    clean_files = 0

    for file_idx, finfo in enumerate(files):
        runs = results_by_file[file_idx]
        n_clean = sum(1 for r in runs if r["response"].strip() == "CLEAN")

        if n_clean == n_runs:
            clean_files += 1
            continue

        merged = aggregate_spans(finfo["path"], finfo["content"], runs)
        for (start, end), info in sorted(merged.items()):
            all_candidates.append({
                "file": finfo["path"],
                "start": start,
                "end": end,
                "lines": end - start + 1,
                "votes": info["count"],
                "out_of": n_runs,
                "content": info["content"],
            })

    print(f"  {clean_files}/{len(files)} files marked CLEAN by all runs")
    print()

    # Phase 4: Report
    # Sort by confidence (votes/n_runs), then file
    high = [c for c in all_candidates if c["votes"] >= 4]
    medium = [c for c in all_candidates if c["votes"] == 3]
    low = [c for c in all_candidates if c["votes"] <= 2]

    print("=" * 60)
    print("TRIAGE REPORT")
    print("=" * 60)
    print()

    print(f"HIGH CONFIDENCE ({len(high)} spans, {n_runs-1}+ runs agree):")
    for c in sorted(high, key=lambda x: x["file"]):
        preview = c["content"].splitlines()[0][:70] if c["content"] else ""
        print(f"  [{c['votes']}/{c['out_of']}] {c['file']}:{c['start']}-{c['end']} ({c['lines']}L)")
        print(f"         {preview}")
    print()

    print(f"MEDIUM CONFIDENCE ({len(medium)} spans, 3/{n_runs} runs):")
    for c in sorted(medium, key=lambda x: x["file"]):
        preview = c["content"].splitlines()[0][:70] if c["content"] else ""
        print(f"  [{c['votes']}/{c['out_of']}] {c['file']}:{c['start']}-{c['end']} ({c['lines']}L)")
        print(f"         {preview}")
    print()

    print(f"LOW CONFIDENCE ({len(low)} spans, ≤2/{n_runs} — likely noise):")
    for c in sorted(low, key=lambda x: x["file"]):
        preview = c["content"].splitlines()[0][:70] if c["content"] else ""
        print(f"  [{c['votes']}/{c['out_of']}] {c['file']}:{c['start']}-{c['end']} ({c['lines']}L)")
        print(f"         {preview}")

    # Output sexp
    sexp_path = "boundary/tools/comment-triage.sexp"
    with open(sexp_path, "w") as f:
        f.write(";; Auto-generated comment triage report (v2 — full-file review)\n")
        f.write(f";; Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f";; Files scanned: {len(files)}\n")
        f.write(f";; Runs per file: {n_runs}\n")
        f.write(f";; Clean files: {clean_files}\n\n")
        f.write("(comment-triage\n")
        for label, group in [("high", high), ("medium", medium), ("low", low)]:
            f.write(f"  ({label}\n")
            for c in sorted(group, key=lambda x: x["file"]):
                f.write(f'    (span (file "{c["file"]}") '
                        f'(lines {c["start"]} {c["end"]}) '
                        f'(votes {c["votes"]} {c["out_of"]}))\n')
            f.write("  )\n")
        f.write(")\n")

    print(f"\nSexp output: {sexp_path}")
    print(f"Total spans: {len(all_candidates)} "
          f"(high: {len(high)}, medium: {len(medium)}, low: {len(low)})")

    return all_candidates


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "review"

    # Parse -n flag
    n_runs = RUNS_PER_FILE
    if "-n" in sys.argv:
        idx = sys.argv.index("-n")
        if idx + 1 < len(sys.argv):
            n_runs = int(sys.argv[idx + 1])

    if cmd == "scan":
        files = find_candidate_files(LATTICE_ROOT)
        print(f"{len(files)} files with commented s-expressions:")
        for f in files:
            print(f"  {f['line_count']:5d}L  {f['path']}")
    elif cmd == "review":
        run_pipeline(n_runs=n_runs)
    else:
        print(__doc__)
