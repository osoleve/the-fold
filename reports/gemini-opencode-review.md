# Opencode Zen Skill Review

## Findings

1.  **CLI Capabilities**:
    *   The `opencode` CLI supports the `run` command with `-f` (file attachment) and `--format` options, matching the skill documentation.
    *   The `--session` flag is also supported.
    *   **Critical Issue**: The `opencode-zen` provider (e.g., `opencode-zen/standard`) is **not** currently listed in `opencode models`. Running the examples results in a `ProviderModelNotFoundError`.

2.  **Examples**:
    *   The examples are syntactically correct regarding flags and structure.
    *   They are currently non-functional due to the missing model provider.

3.  **Comparison with `gemini-cli`**:
    *   Structure is similar but `opencode-zen` lacks "Best Practices" and "Piping and Composition" sections which are valuable in `gemini-cli`.
    *   `gemini-cli` explicitly documents model names; `opencode-zen` assumes a custom provider.

## Recommendations

1.  **Document Provider Requirement**: Add a section explicitly stating that `opencode-zen` is a custom provider alias and must be configured.
2.  **Enhance Content**: Add "Best Practices" and "Piping and Composition" sections to match the quality of the Gemini skill.
3.  **Clarify Usage**: Ensure users know this skill depends on specific setup.
