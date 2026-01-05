#!/usr/bin/env python3
"""
agents/harness/demo.py — Demo of the agent harness

Runs a single step using a real LLM (Gemini Flash) to interact with The Fold.
"""

import subprocess
import json
import sys
import os

# Add parent to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from agents.harness import (
    step,
    generate_status_prompt,
    AgentContext,
    ActionRecord,
)


def gemini_flash_client(prompt: str) -> str:
    """
    Call Gemini Flash via CLI.

    Returns the completion text.
    """
    result = subprocess.run(
        ["gemini", "-m", "gemini-3-flash-preview", prompt],
        capture_output=True,
        text=True,
        timeout=60,
        cwd="/home/oso/the-fold"
    )

    if result.returncode != 0:
        raise Exception(f"Gemini error: {result.stderr}")

    return result.stdout.strip()


def main():
    print("=" * 60)
    print("Agent Harness Demo")
    print("=" * 60)

    # Create agent context
    context = AgentContext(
        session_id="harness-demo",
        main_goal="Explore The Fold forum and find something interesting",
        current_subgoal=None,  # Let the LLM decide
        recent_actions=[],
        environment_notes=[
            "This is a demo of the agent harness",
            "You have access to the Fold REPL",
        ]
    )

    # Generate status prompt
    prompt = generate_status_prompt(context)
    print("\n--- STATUS PROMPT ---")
    print(prompt)
    print("--- END PROMPT ---\n")

    # Run a step
    print("Calling Gemini Flash...")
    result = step(
        session_id="harness-demo",
        status_prompt=prompt,
        api_client=gemini_flash_client,
        retry_budget=3
    )

    # Display results
    print("\n--- STEP RESULT ---")
    print(f"Status: {result.status}")
    print(f"Parsed Action: {result.parsed_action}")
    print(f"Eval Success: {result.eval_success}")
    print(f"\n--- RAW COMPLETION ---")
    print(result.raw_completion[:500])
    print(f"\n--- EVAL RESULT ---")
    if result.eval:
        print(f"Value: {result.eval.value[:1000] if result.eval.value else '(empty)'}")
        if result.eval.output:
            print(f"Output: {result.eval.output[:500]}")
    else:
        print("(no eval performed)")
    print(f"\n--- METRICS ---")
    print(f"  API: {result.metrics.api_duration_ms}ms")
    print(f"  Parse: {result.metrics.parse_duration_ms}ms")
    print(f"  Eval: {result.metrics.eval_duration_ms}ms")
    print(f"  Total: {result.metrics.total_duration_ms}ms")
    print(f"  Retries: {result.metrics.retry_count}")

    if result.error:
        print(f"\n--- ERROR ---")
        print(result.error)

    print("\n" + "=" * 60)
    return result.status == "success"


if __name__ == "__main__":
    os.chdir("/home/oso/the-fold")
    success = main()
    sys.exit(0 if success else 1)
