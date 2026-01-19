# Scaffolding System

The scaffolding system (`boundary/tools/scaffold.ss`) provides code generation from templates to accelerate development in The Fold.

## Quick Start

```scheme
;; Load in REPL
(load "boundary/tools/scaffold.ss")

;; Create a new boundary module
(scaffold 'boundary-module "my-module"
          '((description . "Does cool things")
            (dependencies . "fs.ss, text.ss")))

;; Create a new core module
(scaffold 'core-module "my-pure-fn"
          '((description . "Pure computation")))

;; Create a toolkit tool
(scaffold 'tool "my-analyzer"
          '((category . introspection)
            (description . "Analyzes code patterns")))

;; Interactive mode
(scaffold-interactive)
```

## Built-in Templates

### `boundary-module`
Generates a complete boundary module with:
- Module header with description
- Dependency documentation
- Example functions
- Test file with test framework

**Options:**
- `description`: Module description (default: "A new boundary module")
- `dependencies`: Comma-separated dependency list (default: "fs.ss, text.ss")

### `core-module`
Generates a pure core module with:
- Core code header with totality guarantees
- Dependency loading
- Pure function examples
- Test file with test framework

**Options:**
- `description`: Module description (default: "A new core module")
- `dependencies`: Core dependencies (default: "prelude.ss")

### `tool`
Generates a toolkit tool with:
- Tool interface with help
- Integration points for toolkit.ss
- Test file

**Options:**
- `description`: Tool description
- `category`: One of `building`, `introspection`, `debugging`, `analysis`

### `test-suite`
Generates a standalone test file:
- Test framework loading
- Test groups
- Example tests

**Options:**
- `description`: What is being tested
- `target-file`: File being tested

### `playground`
Generates a playpen creation (game/tool/experiment):
- Dependency loading with guards
- State management
- Main interface functions

**Options:**
- `description`: Playground description
- `type`: One of `game`, `tool`, `experiment`

### `forum-post`
Generates a forum post draft:
- Post metadata
- Instructions for posting
- Editable body

**Options:**
- `channel`: Channel name (default: "engineering")
- `title`: Post title
- `body`: Post content

## Template Variables

All templates support these substitutions:

- `{{NAME}}` - Module/component name as provided
- `{{NAME-UPPER}}` - Uppercase name
- `{{NAME-LOWER}}` - Lowercase name
- `{{DESCRIPTION}}` - Description from options
- `{{AUTHOR}}` - Author from current session
- `{{TIMESTAMP}}` - Current timestamp
- `{{YEAR}}` - Current year
- `{{CUSTOM-VAR}}` - Any custom variable from options

## Creating Custom Templates

```scheme
(define-template
  'my-template-name
  "Template description"
  ;; Variables: ((name . (prompt . default)) ...)
  '((var1 . ("First variable" . "default1"))
    (var2 . ("Second variable" . "default2")))
  ;; Files: (((path . path-template) (content . content-template)) ...)
  '(((path . "output/{{NAME}}.ss")
     (content . "Generated content for {{NAME}}\nVar1: {{VAR1}}"))))
```

## Examples

### Generate a Boundary Module

```scheme
(scaffold 'boundary-module "string-utils"
          '((description . "String manipulation utilities")
            (dependencies . "text.ss")))
```

Creates:
- `boundary/string-utils.ss` - Main module file
- `boundary/test-string-utils.ss` - Test file

### Generate a Core Module

```scheme
(scaffold 'core-module "list-ops"
          '((description . "Pure list operations")
            (dependencies . "prelude.ss")))
```

Creates:
- `core/list-ops.ss` - Pure core module
- `core/test-list-ops.ss` - Test file

### Generate a Toolkit Tool

```scheme
(scaffold 'tool "code-stats"
          '((category . analysis)
            (description . "Analyze code statistics")))
```

Creates:
- `boundary/code-stats.ss` - Tool implementation
- `boundary/test-code-stats.ss` - Test file

**Important:** After generating a tool, manually add it to `boundary/toolkit.ss` registry.

### Generate a Playground Game

```scheme
(scaffold 'playground "dungeon-crawler"
          '((type . game)
            (description . "A text-based dungeon crawler")))
```

Creates:
- `user/templates/dungeon-crawler.ss` - Game implementation

### Interactive Wizard

```scheme
(scaffold-interactive)
```

The wizard will:
1. Show available templates
2. Prompt for template name
3. Prompt for module/component name
4. Prompt for each template variable
5. Generate files

## Help Commands

```scheme
(list-templates)     ; Show all available templates
(scaffold-help)      ; Comprehensive help
```

## After Scaffolding

After generating files:

1. **Review** generated code
2. **Implement** TODOs and placeholder functions
3. **Test** using `scheme --script <test-file>.ss`
4. **Document** any special behavior
5. **Integrate** if needed (e.g., add tools to toolkit.ss)
6. **Commit** when ready

## File Generation

Generated files include:
- Proper headers with description and dependencies
- Author name from current session
- Timestamp of generation
- Placeholder functions to implement
- Test files ready to extend

## Design Principles

1. **Templates are extensible** - Easy to add new templates
2. **Variable substitution** - Flexible placeholder system
3. **Interactive and programmatic** - Both modes supported
4. **Session-aware** - Uses author info from session
5. **Testable** - Full test coverage included

## Testing

Run the scaffolding system tests:

```bash
scheme --script boundary/test-scaffold.ss
```

Tests verify:
- Template registration
- Variable substitution
- File generation
- Built-in templates
- Edge cases

## Integration

The scaffolding system integrates with:
- Session system for author information
- Filesystem capabilities for file writing
- Text utilities for string manipulation
- Timestamp utilities for metadata

## Limitations

- No validation of generated code (run tests to verify)
- File conflicts not checked (will overwrite)
- Custom templates require manual registration
- No undo/rollback (use git to revert)

## Future Enhancements

Potential improvements:
- Template validation before generation
- File conflict detection
- Dry-run mode to preview output
- Template inheritance/composition
- Git integration for auto-commit
- More sophisticated variable transformations
