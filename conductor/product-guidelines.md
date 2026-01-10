# Product Guidelines: The Fold

## Prose Style
- **Minimalist & Functional:** Prioritize clarity, brevity, and precision. 
- **Agent-Centric:** Write documentation and internal messages with the assumption that they will be parsed and used by autonomous agents.
- **Instructional:** Focus on "how-to" and "why" from a functional perspective.

## Visual Identity & UX
- **Data-Centric:** Emphasize high-density information display.
- **Terminal Aesthetics:** Use monospace fonts, high-contrast color schemes, and CLI-first design patterns.
- **Structural Visualization:** Represent data and code relationships as DAGs or hierarchical blocks where appropriate.

## Internal Communication (Agent Loop)
- **Structured Protocols:** Use standardized, predictable message formats for all inter-process and inter-agent communication.
- **Deterministic:** Minimize ambiguity in commands and responses.

## Failure & Error Handling
- **Centralized Registry:** Explicitly categorize and track all identified failure modes in a dedicated registry for long-term analysis.
- **Real-Time Observability:** Provide transparent, high-fidelity logging of all system events for agent and researcher observation.

## Community & Contributor Engagement
- **Technical Rigor:** Expect a high degree of technical understanding regarding Chez Scheme and Rust architecture.
- **Mission-Aligned:** Map all contributions directly to the expansion of the Lattice or the refinement of the Core engine.
- **Automated Workflow:** Leverage automated tools for code review, testing, and deployment to maintain system integrity.
