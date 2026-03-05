;;; skills/optics.sexp — Skill curation file

(skill optics
  (description
    "A complete optics tower with bidirectional transformations for composable\ndata access and reversible migrations. Each optic type provides different\ncapabilities for focusing on parts of data structures:\n\n                  Fold\n                 /    \\\n            Getter    Traversal\n                 \\    /    \\\n                  Affine   Setter\n                 /    \\     |\n              Prism   Lens  |\n                 \\    /    /\n                   Iso ---- Grate\n                        \\   /\n                         \\ /\n\nGrate is the categorical dual of Lens:\n- Lens: get/set a single focus (requires Strong profunctor)\n- Grate: cotraverse/zipWith over a structure (requires Closed profunctor)\n\nKey features:\n- Unified composition that respects the hierarchy (lens+prism=affine, etc.)\n- Law verification for all optic types including Grate\n- Operator syntax (^., ^?, ^.., .~, %~) for ergonomic use\n- Comprehensive instances for Maybe, Either, List, and pairs\n- Automatic type-preserving composition\n- Grate for zipping multiple structures together\n- Bidirectional transformations: migrations with automatic rollback\n- Schema DSL for field-level operations (rename, add, transform)\n- Block migrations for CAS schema evolution\n- Bottom-up tree traversal for Merkle DAG correctness")

  (keywords (optics lenses prisms affines traversals folds getters setters isomorphisms grates data-access composition functional-programming profunctor profunctor-optics strong choice closed cotraverse zipWith bidirectional migrations schema-evolution format-conversion rollback reversible block-migration merkle-dag))

  (aliases (optics lens prism profunctor-optics))

  (concepts (optics-paradigm))

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
