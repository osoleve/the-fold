//! Prelude Module
//!
//! Contains essential higher-order functions and utilities for Fold.
//! The prelude is assembled from multiple .ss module files at compile time.
//!
//! MINIMAL PRELUDE - Only core functionality loaded at startup.
//! Additional functions available via stdlib modules loaded with (load "stdlib/...").
//!
//! Core modules:
//! - core.ss: map, filter, foldl, foldr, any, all, etc.
//! - function.ss: compose, pipe, curry, flip, etc.
//! - equality.ss: eqv?, equal?, member?
//! - maybe.ss: Maybe monad (just, nothing, etc.)
//! - either.ss: Either monad (left, right, ok, err)

use crate::fabric::{Env, EnvRef, EvalOutcome, Symbol, Value, eval_spanned};
use crate::tools::{lower_expr, parse_fold_expr};

// Include minimal module files at compile time
const CORE: &str = include_str!("core.ss");
const FUNCTION: &str = include_str!("function.ss");
const EQUALITY: &str = include_str!("equality.ss");
const MAYBE: &str = include_str!("maybe.ss");
const EITHER: &str = include_str!("either.ss");
const EXPORTS: &str = include_str!("exports-minimal.ss");

/// Assemble the minimal prelude source from module files.
/// Returns a complete Scheme expression that evaluates to an alist of bindings.
fn assemble_prelude() -> String {
    // Build the let* block with all module definitions
    let mut source = String::with_capacity(32 * 1024);

    source.push_str("; ============================================================\n");
    source.push_str("; The Fold Prelude (Minimal)\n");
    source.push_str("; Additional functions: (load \"stdlib/...\")\n");
    source.push_str("; ============================================================\n\n");
    source.push_str("(let*\n  (");

    // Add each module's definitions
    // Order matters: later modules can depend on earlier ones
    source.push_str(CORE);
    source.push('\n');
    source.push_str(FUNCTION);
    source.push('\n');
    source.push_str(EQUALITY);
    source.push('\n');
    source.push_str(MAYBE);
    source.push('\n');
    source.push_str(EITHER);
    source.push('\n');

    source.push_str(")\n  ; Return the export alist\n  ");
    source.push_str(EXPORTS);
    source.push_str("\n)");

    source
}

/// Lazy initialization of the assembled prelude source.
static PRELUDE_SOURCE: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// Get the assembled prelude source (cached after first call).
pub fn prelude_source() -> &'static str {
    PRELUDE_SOURCE.get_or_init(assemble_prelude)
}

/// Load the prelude into an environment.
pub fn load_prelude(env: EnvRef, fuel: usize) -> Result<EnvRef, String> {
    let source = prelude_source();
    let parsed = parse_fold_expr(source, Some("<prelude>"))
        .map_err(|e| format!("prelude parse error: {}", e))?;
    let expr = lower_expr(&parsed).map_err(|e| format!("prelude lower error: {}", e))?;

    let result = match eval_spanned(expr, env.clone(), fuel) {
        Ok(EvalOutcome::Done(value)) => value,
        Ok(EvalOutcome::Suspended { .. }) => {
            return Err("prelude evaluation ran out of fuel".to_string());
        }
        Err(e) => {
            return Err(format!("prelude eval error: {}", e));
        }
    };

    // The prelude returns an alist of (name . value) pairs
    // Insert each binding into the environment
    let mut current = &result;
    loop {
        match current {
            Value::Nil => break,
            Value::Pair(head, tail) => {
                if let Value::Pair(name_box, value_box) = &**head
                    && let Value::Symbol(name) = &**name_box
                {
                    Env::insert(&env, *name, (**value_box).clone());
                }
                current = tail;
            }
            _ => break,
        }
    }

    Ok(env)
}

/// Create a new environment with the prelude loaded.
pub fn prelude_env(fuel: usize) -> Result<EnvRef, String> {
    let env = primitives_env();
    load_prelude(env, fuel)
}

/// All primitive operator names that should be bound as first-class values.
pub const PRIMITIVE_NAMES: &[&str] = &[
    // Arithmetic
    "+",
    "-",
    "*",
    "/",
    "add",
    "sub",
    "mul",
    "div",
    "mod",
    "quotient",
    "remainder",
    "abs",
    "neg",
    "sqrt",
    "expt",
    "pow",
    "floor",
    "ceiling",
    "round",
    "truncate",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "log",
    "exp",
    "min",
    "max",
    "gcd",
    "lcm",
    "clamp",
    "between?",
    "sign",
    "divmod",
    "int->float",
    "float->int",
    "inc",
    "dec",
    "succ",
    "pred",
    // Comparisons
    "<",
    ">",
    "<=",
    ">=",
    "=",
    "eq?",
    "equal?",
    "eqv?",
    // Boolean
    "not",
    "and",
    "or",
    // Type predicates
    "null?",
    "pair?",
    "list?",
    "number?",
    "integer?",
    "float?",
    "string?",
    "symbol?",
    "boolean?",
    "procedure?",
    "char?",
    "vector?",
    "positive?",
    "negative?",
    "zero?",
    "even?",
    "odd?",
    // List primitives
    "car",
    "cdr",
    "cons",
    "list",
    "length",
    "append",
    "reverse",
    "first",
    "second",
    "third",
    "rest",
    "list-ref",
    "list-tail",
    "make-list",
    "memq",
    "memv",
    "member",
    "assq",
    "assv",
    "assoc",
    // String primitives
    "string-append",
    "string-length",
    "string-ref",
    "string=?",
    "string<?",
    "string>?",
    "string<=?",
    "string>=?",
    "string->list",
    "list->string",
    "string->number",
    "number->string",
    "string->symbol",
    "symbol->string",
    "substring",
    "string-split",
    "string-join",
    "string-trim",
    "string-upcase",
    "string-downcase",
    "string-contains?",
    "string-empty?",
    "make-string",
    "string",
    // Character primitives
    "char->integer",
    "integer->char",
    "char=?",
    "char<?",
    "char>?",
    "char<=?",
    "char>=?",
    "char-upcase",
    "char-downcase",
    "char-alphabetic?",
    "char-numeric?",
    "char-whitespace?",
    // Vector primitives
    "vector",
    "make-vector",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "vector->list",
    "list->vector",
    // Bitwise
    "bitand",
    "bitor",
    "bitxor",
    "bitnot",
    "bitshl",
    "bitshr",
    // Control
    "if",
    "cond",
    "case",
    "when",
    "unless",
    "let",
    "let*",
    "letrec",
    "lambda",
    "fn",
    "fix",
    "begin",
    "do",
    // Quoting
    "quote",
    "quasiquote",
    "unquote",
    "unquote-splicing",
    // Application
    "apply",
    "eval",
    "load",
    // I/O
    "display",
    "write",
    "newline",
    "read",
    "read-file",
    "write-file",
    "file-exists?",
    // Misc
    "gensym",
    "void",
    "error",
    "identity",
];

/// Create an environment with all primitives bound as first-class values.
pub fn primitives_env() -> EnvRef {
    let env = Env::new();
    for &name in PRIMITIVE_NAMES {
        Env::insert(
            &env,
            Symbol::intern(name),
            Value::Primitive(Symbol::intern(name)),
        );
    }
    env
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prelude_assembles() {
        let source = prelude_source();
        assert!(source.len() > 1000, "prelude should be substantial");
        assert!(source.contains("(let*"), "prelude should be a let* block");
        assert!(source.contains("map"), "prelude should contain map");
        assert!(source.contains("filter"), "prelude should contain filter");
    }

    #[test]
    fn test_prelude_parses() {
        let source = prelude_source();
        let result = parse_fold_expr(source, Some("<prelude>"));
        assert!(result.is_ok(), "prelude should parse: {:?}", result.err());
    }

    #[test]
    fn test_prelude_lowers() {
        let source = prelude_source();
        let parsed = parse_fold_expr(source, Some("<prelude>")).expect("prelude should parse");
        let result = lower_expr(&parsed);
        assert!(result.is_ok(), "prelude should lower: {:?}", result.err());
    }
}
