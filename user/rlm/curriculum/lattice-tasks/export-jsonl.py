#!/usr/bin/env python3
"""Export lattice development tasks from S-expression format to JSONL.

Reads .sexp task files, extracts task records, and emits one JSON line per task
in Verifiers format: {prompt, answer, task, info, example_id}.

Usage:
    python export-jsonl.py [output.jsonl]
    python export-jsonl.py  # defaults to dev-tasks.jsonl
"""

import json
import os
import sys
from pathlib import Path


# ---------------------------------------------------------------------------
# Minimal S-expression parser
# ---------------------------------------------------------------------------

def tokenize(text):
    """Tokenize S-expression text into (type, value) pairs."""
    tokens = []
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c in ' \t\n\r':
            i += 1
        elif c == ';':
            while i < n and text[i] != '\n':
                i += 1
        elif c == '(':
            tokens.append(('LPAREN', None))
            i += 1
        elif c == ')':
            tokens.append(('RPAREN', None))
            i += 1
        elif c == '"':
            j = i + 1
            chars = []
            while j < n:
                if text[j] == '\\' and j + 1 < n:
                    esc = text[j + 1]
                    if esc == 'n':
                        chars.append('\n')
                    elif esc == 't':
                        chars.append('\t')
                    elif esc == '"':
                        chars.append('"')
                    elif esc == '\\':
                        chars.append('\\')
                    else:
                        chars.append(esc)
                    j += 2
                elif text[j] == '"':
                    break
                else:
                    chars.append(text[j])
                    j += 1
            tokens.append(('STRING', ''.join(chars)))
            i = j + 1
        elif c == '#':
            if i + 1 < n and text[i + 1] in 'tf':
                tokens.append(('BOOL', text[i + 1] == 't'))
                i += 2
            else:
                j = i
                while j < n and text[j] not in ' \t\n\r()";':
                    j += 1
                tokens.append(('SYMBOL', text[i:j]))
                i = j
        elif c == "'":
            tokens.append(('QUOTE', None))
            i += 1
        else:
            j = i
            while j < n and text[j] not in ' \t\n\r()";':
                j += 1
            atom = text[i:j]
            try:
                tokens.append(('NUMBER', int(atom)))
            except ValueError:
                try:
                    tokens.append(('NUMBER', float(atom)))
                except ValueError:
                    tokens.append(('SYMBOL', atom))
            i = j
    return tokens


def parse_tokens(tokens, pos):
    """Parse tokens starting at pos, return (value, new_pos)."""
    if pos >= len(tokens):
        return None, pos
    typ, val = tokens[pos]
    if typ == 'LPAREN':
        lst = []
        pos += 1
        while pos < len(tokens) and tokens[pos][0] != 'RPAREN':
            elem, pos = parse_tokens(tokens, pos)
            if elem is not None:
                lst.append(elem)
        return lst, pos + 1  # skip RPAREN
    elif typ == 'QUOTE':
        elem, pos = parse_tokens(tokens, pos + 1)
        return ['quote', elem], pos
    elif typ == 'STRING':
        return val, pos + 1
    elif typ == 'NUMBER':
        return val, pos + 1
    elif typ == 'BOOL':
        return val, pos + 1
    elif typ == 'SYMBOL':
        return val, pos + 1
    else:
        return None, pos + 1


def parse_sexp(text):
    """Parse S-expression text into Python nested lists/atoms."""
    tokens = tokenize(text)
    results = []
    pos = 0
    while pos < len(tokens):
        val, pos = parse_tokens(tokens, pos)
        if val is not None:
            results.append(val)
    return results[0] if len(results) == 1 else results


# ---------------------------------------------------------------------------
# Task extraction
# ---------------------------------------------------------------------------

def extract_field(task_sexp, field_name):
    """Extract a field value from a task S-expression.

    Task format: (task (field1 value1) (field2 value2) ...)
    """
    for item in task_sexp[1:]:  # skip 'task' symbol
        if isinstance(item, list) and len(item) >= 2 and item[0] == field_name:
            if len(item) == 2:
                return item[1]
            return item[1:]
    return None


def extract_tasks(sexp):
    """Extract task records from a parsed (tasks ...) form."""
    if not isinstance(sexp, list) or sexp[0] != 'tasks':
        return []
    tasks = []
    for item in sexp[1:]:
        if isinstance(item, list) and len(item) > 0 and item[0] == 'task':
            tasks.append(item)
    return tasks


def flatten_list(val):
    """Ensure val is a flat list of strings."""
    if val is None:
        return []
    if isinstance(val, str):
        return [val]
    if isinstance(val, list):
        result = []
        for item in val:
            if isinstance(item, list):
                result.extend(flatten_list(item))
            elif isinstance(item, str):
                result.append(item)
        return result
    return []


def build_prompt(task):
    """Build the model prompt from a task record."""
    task_id = extract_field(task, 'id')
    module = extract_field(task, 'module')
    task_type = extract_field(task, 'type')
    description = extract_field(task, 'description')
    context = extract_field(task, 'context')
    before = extract_field(task, 'before')

    # Available modules
    avail = extract_field(task, 'available-modules')
    avail_str = ', '.join(str(m) for m in avail) if isinstance(avail, list) else str(avail or 'prelude')

    lines = [
        f"You are writing Scheme code for the Fold lattice.",
        f"",
        f"Module: {module}",
        f"Available modules: {avail_str}",
        f"",
        f"## Task ({task_type})",
        f"",
        f"{description}",
    ]

    if context:
        lines.extend(["", f"## Context", "", f"{context}"])

    if before and str(before).strip() and not str(before).strip().startswith(";;"):
        lines.extend(["", f"## Starting code", "", f"```scheme", f"{before}", f"```"])
    elif before and str(before).strip():
        lines.extend(["", f"## Notes", "", f"{before}"])

    lines.extend([
        "",
        "## Instructions",
        "",
        "Write the complete Scheme code. Output ONLY the code, nothing else.",
        "For implement/fix tasks: output a complete (define ...) form.",
        "For test tasks: output test code using assert-equal, assert-true, assert-false.",
    ])

    return '\n'.join(lines)


def task_to_jsonl(task):
    """Convert a task S-expression to a Verifiers JSONL dict."""
    task_id = extract_field(task, 'id') or "unknown"
    module = str(extract_field(task, 'module') or "")
    tier = extract_field(task, 'tier') or 0
    task_type = str(extract_field(task, 'type') or "implement")
    description = extract_field(task, 'description') or ""
    context = extract_field(task, 'context') or ""
    before = extract_field(task, 'before') or ""
    test_forms = flatten_list(extract_field(task, 'test-forms'))
    reference = extract_field(task, 'reference-solution') or ""
    alternatives = flatten_list(extract_field(task, 'alternatives'))
    hints = flatten_list(extract_field(task, 'hints'))
    difficulty = extract_field(task, 'difficulty') or 3
    avail = extract_field(task, 'available-modules')
    avail_modules = [str(m) for m in avail] if isinstance(avail, list) else ["prelude"]

    prompt = build_prompt(task)

    info = {
        "module": module,
        "tier": tier,
        "type": task_type,
        "description": description,
        "context": context,
        "before": before,
        "test_forms": test_forms,
        "available_modules": avail_modules,
        "hints": hints,
        "difficulty": difficulty,
        "alternatives": alternatives,
    }

    return {
        "prompt": prompt,
        "answer": reference,
        "task": task_type,
        "info": json.dumps(info),
        "example_id": task_id,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    output_path = sys.argv[1] if len(sys.argv) > 1 else "dev-tasks.jsonl"

    # Find task files from index.sexp
    script_dir = Path(__file__).parent
    index_path = script_dir / "index.sexp"

    if not index_path.exists():
        print(f"Error: {index_path} not found", file=sys.stderr)
        sys.exit(1)

    index_text = index_path.read_text()
    index_sexp = parse_sexp(index_text)

    # Extract module entries from index
    task_files = []
    def find_modules(sexp):
        if isinstance(sexp, list):
            if len(sexp) > 0 and sexp[0] == 'modules':
                for entry in sexp[1:]:
                    if isinstance(entry, list) and len(entry) >= 4:
                        # (module-name tier deps task-file status)
                        task_file = entry[3]
                        if isinstance(task_file, str) and task_file.endswith('.sexp'):
                            task_files.append(task_file)
            else:
                for item in sexp:
                    find_modules(item)

    find_modules(index_sexp)

    if not task_files:
        print("Error: no task files found in index.sexp", file=sys.stderr)
        sys.exit(1)

    # Parse each task file and collect tasks
    all_tasks = []
    for task_file in task_files:
        path = script_dir / task_file
        if not path.exists():
            print(f"Warning: {path} not found, skipping", file=sys.stderr)
            continue
        text = path.read_text()
        sexp = parse_sexp(text)
        tasks = extract_tasks(sexp)
        all_tasks.extend(tasks)
        print(f"  {task_file}: {len(tasks)} tasks", file=sys.stderr)

    # Convert to JSONL
    output_file = script_dir / output_path
    with open(output_file, 'w') as f:
        for task in all_tasks:
            record = task_to_jsonl(task)
            f.write(json.dumps(record) + '\n')

    print(f"\nExported {len(all_tasks)} tasks to {output_file}", file=sys.stderr)


if __name__ == '__main__':
    main()
