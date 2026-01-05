#!/usr/bin/env python3
"""
agents/harness/run.py — Run an agent with full observability

Usage:
    python -m agents.harness.run "Your goal here"
    python -m agents.harness.run --steps 5 "Simple goal"
    python -m agents.harness.run -p gemini -m gemini-3-pro-preview "Complex goal"
    python -m agents.harness.run -p groq -m llama-3.3-70b-versatile "Goal"
    python -m agents.harness.run --verify "34" "How many posts in #art?"

Providers:
    gemini  - Google Gemini (via gemini CLI)
    groq    - Groq API (requires GROQ_API_KEY env var)
"""

import argparse
import json
import subprocess
import sys
import os
import urllib.request
import urllib.error

# Add parent to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from agents.harness import (
    AgentContext,
    LoopConfig,
    run_observed,
)


def fold_eval(session: str, code: str) -> str:
    """Evaluate code in a Fold session, return result."""
    input_json = json.dumps({"code": code})
    result = subprocess.run(
        ["./fold-agent.py", "--json", "--session", session],
        input=input_json,
        capture_output=True,
        text=True,
        timeout=30,
        cwd="/home/oso/the-fold"
    )
    try:
        response = json.loads(result.stdout)
        return response.get("result", "")
    except:
        return ""


def init_answer_slot(session: str):
    """Initialize the *answer* variable in the session."""
    fold_eval(session, '(define *answer* #f)')


def read_answer(session: str) -> str:
    """Read the *answer* variable from the session."""
    return fold_eval(session, '*answer*')


def create_gemini_client(model: str = "gemini-3-flash-preview"):
    """Create a Gemini API client."""
    def client(prompt: str) -> str:
        # The status prompt already contains full instructions.
        # Just add a minimal system prefix for the model.
        augmented = """You are an autonomous Scheme agent playing in The Fold.

CRITICAL: Respond with ONLY a single S-expression. No prose, no explanation.

The status prompt below explains your available commands:
- Free actions: (think ...), (note ...), (env ...)
- Registers: (set! *register* value) - use *answer* when you have the final answer

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


def create_groq_client(model: str = "llama-3.3-70b-versatile"):
    """Create a Groq API client using raw HTTP."""
    api_key = os.environ.get("GROQ_API_KEY")
    if not api_key:
        raise ValueError("GROQ_API_KEY environment variable not set")

    def client(prompt: str) -> str:
        system_msg = """Respond with ONE S-expression. No text, no markdown.

To compute: (env (+ 2 2))
To answer: (set! *answer* "4")

Do NOT nest these. Pick one per response."""

        payload = {
            "model": model,
            "messages": [
                {"role": "system", "content": system_msg},
                {"role": "user", "content": prompt}
            ],
            "temperature": 0,
            "max_tokens": 1024,
        }

        req = urllib.request.Request(
            "https://api.groq.com/openai/v1/chat/completions",
            data=json.dumps(payload).encode("utf-8"),
            headers={
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json",
                "User-Agent": "fold-agent/1.0",
            },
            method="POST"
        )

        try:
            with urllib.request.urlopen(req, timeout=60) as resp:
                result = json.loads(resp.read().decode("utf-8"))
                return result["choices"][0]["message"]["content"].strip()
        except urllib.error.HTTPError as e:
            error_body = e.read().decode("utf-8") if e.fp else ""
            raise Exception(f"Groq API error {e.code}: {error_body}")

    return client


# Available providers and their default models
PROVIDERS = {
    "gemini": ("gemini-3-flash-preview", create_gemini_client),
    "groq": ("llama-3.3-70b-versatile", create_groq_client),
}


def main():
    parser = argparse.ArgumentParser(description="Run an agent with observability")
    parser.add_argument("goal", help="The goal for the agent")
    parser.add_argument("--steps", type=int, default=15, help="Maximum steps (default: 15)")
    parser.add_argument("--provider", "-p", choices=list(PROVIDERS.keys()), default="gemini",
                        help="LLM provider (default: gemini)")
    parser.add_argument("--model", "-m", default=None, help="Model name (provider-specific default if not set)")
    parser.add_argument("--session", default="observed-run", help="Session ID")
    parser.add_argument("--no-transcript", action="store_true", help="Don't save transcript")
    parser.add_argument("--quiet", action="store_true", help="Minimal output")
    parser.add_argument("--verify", metavar="EXPECTED", help="Verify answer against expected value")

    args = parser.parse_args()

    os.chdir("/home/oso/the-fold")

    # Initialize the answer slot in the Fold session
    init_answer_slot(args.session)

    # Create context
    context = AgentContext(
        session_id=args.session,
        main_goal=args.goal,
        current_subgoal=None,
        recent_actions=[],
        environment_notes=[
            "You are an autonomous agent exploring The Fold",
            "Set subgoals to break down your task",
            "Write your final answer to *answer* with (set! *answer* \"...\") then call (done)",
        ]
    )

    # Create config
    config = LoopConfig(
        max_steps=args.steps,
        stop_on_done=True,
        detect_loops=True,
    )

    # Create API client
    default_model, client_factory = PROVIDERS[args.provider]
    model = args.model or default_model
    api_client = client_factory(model)

    if not args.quiet:
        print(f"Using {args.provider} with model {model}")

    # Run with observability
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

    # Read the final answer from Fold
    final_answer = read_answer(args.session)

    print("=" * 50)
    print(f"FINAL ANSWER: {final_answer}")
    print("=" * 50)

    # Verify if requested
    if args.verify:
        expected = args.verify
        # Normalize for comparison (strip quotes if present)
        actual = final_answer.strip().strip('"')
        expected = expected.strip().strip('"')

        if actual == expected:
            print(f"✓ VERIFIED: Answer matches expected value")
            return 0
        else:
            print(f"✗ MISMATCH: Expected '{expected}', got '{actual}'")
            return 1

    # Exit code based on completion
    if loop_result and loop_result.finish_reason == "done":
        return 0
    else:
        return 1


if __name__ == "__main__":
    sys.exit(main())
