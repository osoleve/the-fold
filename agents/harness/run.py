#!/usr/bin/env python3
"""
agents/harness/run.py — Run an agent with full observability

Usage:
    python -m agents.harness.run "Your goal here"
    python -m agents.harness.run --steps 5 "Simple goal"
    python -m agents.harness.run --model gemini-3-pro-preview "Complex goal"
"""

import argparse
import subprocess
import sys
import os

# Add parent to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from agents.harness import (
    AgentContext,
    LoopConfig,
    run_observed,
)


def create_gemini_client(model: str = "gemini-3-flash-preview"):
    """Create a Gemini API client."""
    def client(prompt: str) -> str:
        augmented = """You are an autonomous Scheme agent in The Fold REPL.

HARNESS COMMANDS (handled locally, not sent to Fold):
  (subgoal "description")  ; Set your current subgoal
  (done)                   ; Signal task completion

CRITICAL: Respond with ONLY a single S-expression. No text before or after.

""" + prompt

        result = subprocess.run(
            ["gemini", "-m", model, augmented],
            capture_output=True,
            text=True,
            timeout=90,
            cwd="/home/oso/the-fold"
        )

        if result.returncode != 0:
            raise Exception(f"Gemini error: {result.stderr}")

        return result.stdout.strip()

    return client


def main():
    parser = argparse.ArgumentParser(description="Run an agent with observability")
    parser.add_argument("goal", help="The goal for the agent")
    parser.add_argument("--steps", type=int, default=15, help="Maximum steps (default: 15)")
    parser.add_argument("--model", default="gemini-3-flash-preview", help="Gemini model to use")
    parser.add_argument("--session", default="observed-run", help="Session ID")
    parser.add_argument("--no-transcript", action="store_true", help="Don't save transcript")
    parser.add_argument("--quiet", action="store_true", help="Minimal output")

    args = parser.parse_args()

    # Create context
    context = AgentContext(
        session_id=args.session,
        main_goal=args.goal,
        current_subgoal=None,
        recent_actions=[],
        environment_notes=[
            "You are an autonomous agent exploring The Fold",
            "Set subgoals to break down your task",
            "Use (done) when you have completed the main goal",
        ]
    )

    # Create config
    config = LoopConfig(
        max_steps=args.steps,
        stop_on_done=True,
        detect_loops=True,
    )

    # Create API client
    api_client = create_gemini_client(args.model)

    # Run with observability
    os.chdir("/home/oso/the-fold")

    loop_result, observer = run_observed(
        context=context,
        api_client=api_client,
        config=config,
        verbose=not args.quiet,
        save_transcript=not args.no_transcript
    )

    # Print summary
    if not args.quiet:
        observer.print_summary()

    # Exit code based on completion
    if loop_result and loop_result.finish_reason == "done":
        return 0
    else:
        return 1


if __name__ == "__main__":
    sys.exit(main())
