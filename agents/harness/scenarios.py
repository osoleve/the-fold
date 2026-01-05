"""
agents/harness/scenarios.py — Test scenarios for agent harness tuning

Categories:
1. Direct computation (baseline)
2. Multi-step computation
3. Information retrieval (requires parsing)
4. Exploration tasks
5. Creative tasks
"""

SCENARIOS = [
    # --- Category 1: Direct computation (should work) ---
    {
        "id": "math-simple",
        "goal": "What is 7 * 8? Set *answer* to the result.",
        "expected": "56",
        "category": "direct",
        "difficulty": 1,
    },
    {
        "id": "math-nested",
        "goal": "Compute (+ (* 3 4) (* 5 6)). Set *answer* to the result.",
        "expected": "42",
        "category": "direct",
        "difficulty": 1,
    },
    {
        "id": "factorial",
        "goal": "Compute the factorial of 7. Set *answer* to the result.",
        "expected": "5040",
        "category": "direct",
        "difficulty": 2,
    },

    # --- Category 2: Multi-step computation ---
    {
        "id": "fibonacci",
        "goal": "Compute the 12th fibonacci number (fib(0)=0, fib(1)=1). Set *answer* to the result.",
        "expected": "144",
        "category": "multi-step",
        "difficulty": 2,
    },
    {
        "id": "sum-squares",
        "goal": "Compute the sum of squares from 1 to 5 (1+4+9+16+25). Set *answer* to the result.",
        "expected": "55",
        "category": "multi-step",
        "difficulty": 2,
    },

    # --- Category 3: Information retrieval (harder - requires parsing) ---
    {
        "id": "channel-count",
        "goal": "Run (channels) and find how many posts are in #art. Set *answer* to the number.",
        "expected": "34",  # Will need to update based on actual state
        "category": "retrieval",
        "difficulty": 3,
    },
    {
        "id": "session-info",
        "goal": "Run (who) and extract your session ID. Set *answer* to it.",
        "expected": None,  # Dynamic
        "category": "retrieval",
        "difficulty": 3,
    },

    # --- Category 4: Exploration ---
    {
        "id": "discover-commands",
        "goal": "Use (help) to find the command for posting to the forum. Set *answer* to the command name (just the symbol).",
        "expected": "msg",
        "category": "exploration",
        "difficulty": 2,
    },

    # --- Category 5: Creative (open-ended) ---
    {
        "id": "greeting",
        "goal": "Compose a short greeting message and set *answer* to it.",
        "expected": None,  # Any non-empty string
        "category": "creative",
        "difficulty": 1,
    },
]

# Few-shot examples for the system prompt
FEW_SHOT_EXAMPLES = """
Example 1 - Simple math:
User prompt shows: GOAL: What is 3 + 4?
Response: (env (+ 3 4))
[Result: 7]
Response: (set! *answer* "7")

Example 2 - Define and use:
User prompt shows: GOAL: Compute factorial of 5
Response: (env (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))
Response: (env (fact 5))
[Result: 120]
Response: (set! *answer* "120")

Example 3 - Extract from output:
User prompt shows: GOAL: How many posts in #art?
Response: (env (channels))
[Result shows: #art: 34 posts, 3 authors]
Response: (set! *answer* "34")
"""


def get_scenario(scenario_id: str) -> dict:
    """Get a scenario by ID."""
    for s in SCENARIOS:
        if s["id"] == scenario_id:
            return s
    raise ValueError(f"Unknown scenario: {scenario_id}")


def list_scenarios(category: str = None) -> list:
    """List scenarios, optionally filtered by category."""
    if category:
        return [s for s in SCENARIOS if s["category"] == category]
    return SCENARIOS


if __name__ == "__main__":
    print("Available scenarios:")
    for s in SCENARIOS:
        print(f"  [{s['difficulty']}] {s['id']:20} ({s['category']}) - {s['goal'][:50]}...")
