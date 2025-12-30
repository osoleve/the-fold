//! Prelude Module
//!
//! Contains essential higher-order functions and utilities for Fold.
//! The prelude is assembled from multiple .ss module files at compile time.
//!
//! Module organization:
//! - core.ss: map, filter, foldl, foldr, any, all, etc.
//! - function.ss: compose, pipe, curry, flip, etc.
//! - list.ss: list manipulation functions
//! - equality.ss: eqv?, equal?, member?
//! - maybe.ss: Maybe monad (just, nothing, etc.)
//! - either.ss: Either monad (left, right, ok, err)
//! - numeric.ss: math, statistics, number theory
//! - comparison.ss: sorting, searching, selection
//! - string.ss: string HOFs and utilities
//! - collection.ss: dict, set, alist operations
//! - control.ss: control flow, iteration
//! - validation.ss: assertions, testing, validators
//! - stream.ss: lazy stream utilities
//! - datastructures.ss: queue, stack, deque, priority queue
//! - graph.ss: graph algorithms (BFS, DFS, etc.)
//! - tree.ss: tree traversal and manipulation
//! - missing.ss: comprehensive collection of remaining utility functions
//! - path.ss: filesystem path manipulation
//! - encoding.ss: base64, hex, checksums
//! - compat.ss: Scheme compatibility aliases
//! - exports.ss: final export alist

use crate::fabric::{Env, EnvRef, EvalOutcome, Value, eval_spanned};
use crate::tools::{lower_expr, parse_fold_expr};

// Include module files at compile time
const CORE: &str = include_str!("core.ss");
const FUNCTION: &str = include_str!("function.ss");
const LIST: &str = include_str!("list.ss"); // Marker file
const LIST_CORE: &str = include_str!("list-core.ss");
const LIST_SEARCH: &str = include_str!("list-search.ss");
const LIST_TRANSFORM: &str = include_str!("list-transform.ss");
const LIST_PARTITION: &str = include_str!("list-partition.ss");
const EQUALITY: &str = include_str!("equality.ss");
const MAYBE: &str = include_str!("maybe.ss");
const EITHER: &str = include_str!("either.ss");
const NUMERIC: &str = include_str!("numeric.ss");
const COMPARISON: &str = include_str!("comparison.ss");
const STRING: &str = include_str!("string.ss");
const COLLECTION: &str = include_str!("collection.ss");
const CONTROL: &str = include_str!("control.ss");
const VALIDATION: &str = include_str!("validation.ss");
const STREAM: &str = include_str!("stream.ss");
const DATASTRUCTURES: &str = include_str!("datastructures.ss");
const GRAPH: &str = include_str!("graph.ss");
const TREE: &str = include_str!("tree.ss");
const STATS: &str = include_str!("stats.ss");
const COMBINATORICS: &str = include_str!("combinatorics.ss");
const LIST_EXT: &str = include_str!("list-ext.ss");
const STRING_EXT: &str = include_str!("string-ext.ss");
const GRAPH_EXT: &str = include_str!("graph-ext.ss");
const ALIST_EXT: &str = include_str!("alist-ext.ss");
const NUMERIC_EXT: &str = include_str!("numeric-ext.ss");
const SET_EXT: &str = include_str!("set-ext.ss");
const LOGIC_EXT: &str = include_str!("logic-ext.ss");
const MONAD_CORE: &str = include_str!("monad-core.ss");
const MISSING: &str = include_str!("missing.ss");
const PATH: &str = include_str!("path.ss");
const ENCODING: &str = include_str!("encoding.ss");
const COMPAT: &str = include_str!("compat.ss");
const EXPORTS: &str = include_str!("exports.ss");

/// Assemble the prelude source from module files.
/// Returns a complete Scheme expression that evaluates to an alist of bindings.
fn assemble_prelude() -> String {
    // Build the let* block with all module definitions
    let mut source = String::with_capacity(256 * 1024);

    source.push_str("; ============================================================\n");
    source.push_str("; The Fold Prelude - Assembled from modular components\n");
    source.push_str("; ============================================================\n\n");
    source.push_str("(let*\n  (");

    // Add each module's definitions
    // Order matters: later modules can depend on earlier ones
    source.push_str(CORE);
    source.push('\n');
    source.push_str(FUNCTION);
    source.push('\n');
    source.push_str(LIST); // Marker comment
    source.push('\n');
    source.push_str(LIST_CORE);
    source.push('\n');
    source.push_str(LIST_SEARCH);
    source.push('\n');
    source.push_str(LIST_TRANSFORM);
    source.push('\n');
    source.push_str(LIST_PARTITION);
    source.push('\n');
    source.push_str(EQUALITY);
    source.push('\n');
    source.push_str(MAYBE);
    source.push('\n');
    source.push_str(EITHER);
    source.push('\n');
    source.push_str(NUMERIC);
    source.push('\n');
    source.push_str(COMPARISON);
    source.push('\n');
    source.push_str(STRING);
    source.push('\n');
    source.push_str(COLLECTION);
    source.push('\n');
    source.push_str(CONTROL);
    source.push('\n');
    source.push_str(VALIDATION);
    source.push('\n');
    source.push_str(STREAM);
    source.push('\n');
    source.push_str(DATASTRUCTURES);
    source.push('\n');
    source.push_str(GRAPH);
    source.push('\n');
    source.push_str(TREE);
    source.push('\n');
    source.push_str(STATS);
    source.push('\n');
    source.push_str(COMBINATORICS);
    source.push('\n');
    source.push_str(LIST_EXT);
    source.push('\n');
    source.push_str(STRING_EXT);
    source.push('\n');
    source.push_str(GRAPH_EXT);
    source.push('\n');
    source.push_str(ALIST_EXT);
    source.push('\n');
    source.push_str(NUMERIC_EXT);
    source.push('\n');
    source.push_str(SET_EXT);
    source.push('\n');
    source.push_str(LOGIC_EXT);
    source.push('\n');
    source.push_str(MONAD_CORE);
    source.push('\n');
    source.push_str(MISSING);
    source.push('\n');
    source.push_str(PATH);
    source.push('\n');
    source.push_str(ENCODING);
    source.push('\n');
    source.push_str(COMPAT);
    source.push('\n');

    source.push_str(")\n  ; Return the export alist\n  ");
    source.push_str(EXPORTS);
    source.push_str("\n)");

    source
}

/// Lazy initialization of the assembled prelude source.
/// Uses std::sync::OnceLock for thread-safe lazy initialization.
fn prelude_source() -> &'static str {
    use std::sync::OnceLock;
    static PRELUDE: OnceLock<String> = OnceLock::new();
    PRELUDE.get_or_init(assemble_prelude)
}

/// Load the prelude into an environment, returning the augmented environment.
/// The prelude returns an alist of (name . value) pairs which are inserted into the env.
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
                    Env::insert(&env, name.clone(), (**value_box).clone());
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
/// These can be passed directly to higher-order functions like foldl.
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
    "truncate",
    "inc",
    "dec",
    "succ",
    "pred",
    "sq",
    "cube",
    "double",
    "halve",
    "negate",
    "zero",
    "one",
    "scale",
    "denormalize",
    "lerp",
    "inverse-lerp",
    "smoothstep",
    "saturation-add",
    "percent-of",
    "percent-change",
    "round-to",
    // Rational operations
    "make-rational",
    "rational",
    "rational?",
    "rational-zero?",
    "rational-positive?",
    "rational-negative?",
    "rational-integer?",
    "rational-numerator",
    "rational-denominator",
    "rational-add",
    "rational-sub",
    "rational-mul",
    "rational-div",
    "rational-abs",
    "rational-neg",
    "rational-recip",
    "rational-floor",
    "rational-ceiling",
    "rational-round",
    "rational-truncate",
    "rational->float",
    "rational<?",
    "rational<=?",
    "rational=?",
    "rational>=?",
    "rational>?",
    // Comparison
    "<",
    ">",
    "<=",
    ">=",
    "=",
    "eq?",
    "lt?",
    "gt?",
    "le?",
    "ge?",
    // Type checks and predicates
    "number?",
    "integer?",
    "string?",
    "list?",
    "pair?",
    "null?",
    "boolean?",
    "symbol?",
    "procedure?",
    "char?",
    "vector?",
    "bytevector?",
    "zero?",
    "positive?",
    "negative?",
    "even?",
    "odd?",
    // Bitwise operations
    "bitand",
    "bitor",
    "bitxor",
    "bitnot",
    "shl",
    "shr",
    "atom?",
    "atomic?",
    "not-null?",
    "proper-list?",
    "pair-list?",
    // Symbol operations
    "symbol-upcase",
    "symbol-downcase",
    "symbol-length",
    "symbol-concat",
    // Bytevector operations
    "bytevector-length",
    "bytevector-ref",
    "bytevector->string",
    "string->bytevector",
    // Association list operations
    "alist-get",
    "alist-set",
    "alist-keys",
    "alist-values",
    // List operations
    "car",
    "cdr",
    "cons",
    "set-car!",
    "set-cdr!",
    "list",
    "append",
    "length",
    "reverse",
    "member",
    "memq",
    "list-ref",
    "list->string",
    "list->vec",
    "take",
    "drop",
    "last",
    "init",
    "flatten",
    "range",
    "list-copy",
    "make-list",
    "first",
    "rest",
    "second",
    "third",
    "head",
    "tail",
    "repeat",
    "sum",
    "product",
    "sum-of",
    "max-of",
    "min-of",
    "sort",
    "unique",
    "find-index",
    "index-of",
    "count",
    "find",
    "position",
    "contains?",
    "min-list",
    "max-list",
    "nth",
    "zip",
    "unzip",
    "rotate",
    "list->set",
    "interleave",
    "list->pairs",
    "list->symbols",
    "list->numbers",
    "list-indices",
    "list-reverse-indices",
    "list-length-of",
    // String operations
    "string-length",
    "string-ref",
    "string-append",
    "substring",
    "string->list",
    "string-split",
    "string-join",
    "string-concat",
    "string-repeat",
    "string-reverse",
    "string-upcase",
    "string-downcase",
    "string-trim",
    "string-trim-left",
    "string-trim-right",
    "string-starts-with?",
    "string-ends-with?",
    "string-contains?",
    "string-index-of",
    "string-replace",
    "string-replace-all",
    "string-pad-left",
    "string-pad-right",
    "string=?",
    "string<?",
    "string<=?",
    "string>?",
    "string>=?",
    "number->string",
    "string->number",
    "symbol->string",
    "string->symbol",
    "char-upcase",
    "char-downcase",
    "char-alphabetic?",
    "char-numeric?",
    "char-whitespace?",
    "char-upper-case?",
    "char-lower-case?",
    // Vector operations
    "vec-make",
    "vec-length",
    "vec-ref",
    "vec-set!",
    "vec-map",
    "vec-filter",
    "vec-reduce",
    "vec->list",
    "vec-append",
    "vec-take",
    "vec-drop",
    "vec-slice",
    // BigInt operations
    "bigint",
    "bigint?",
    "bigint-add",
    "bigint-sub",
    "bigint-mul",
    "bigint-div",
    "bigint-mod",
    "bigint-pow",
    "bigint-abs",
    "bigint-neg",
    "bigint-gcd",
    "bigint-lcm",
    "bigint<?",
    "bigint<=?",
    "bigint=?",
    "bigint>=?",
    "bigint>?",
    "bigint-zero?",
    "bigint-positive?",
    "bigint-negative?",
    "bigint-even?",
    "bigint-odd?",
    "bigint->int",
    "int->bigint",
    "bigint->string",
    "string->bigint",
    // Block/CAS operations
    "make-block",
    "block?",
    "block-tag",
    "block-payload",
    "block-refs",
    "block-hash",
    "hash-bytes",
    "fetch",
    "store!",
    "pin!",
    "pinned?",
    // Byte operations
    "bv-make",
    "bv-length",
    "bv-ref",
    "bv-slice",
    "bv-concat",
    "bv-copy",
    "bytes->block",
    // Character operations
    "integer->char",
    "char->integer",
    "char=?",
    "char<?",
    // Logic
    "and",
    "or",
    "not",
    // Logical operations
    "xor",
    "gensym",
    // Conditional and logical utilities
    "when",
    "unless",
    "implies",
    "iff",
    "nand",
    "nor",
    "if",
    "cond-value",
    "coalesce",
    "default-value",
    "both?",
    "either?",
    // Utilities
    "identity",
    "apply",
    "error",
    "deep-equal?",
    "list-every?",
    "assq",
    "assoc",
    // List analysis utilities
    "list-count",
    "list-sum",
    "list-avg",
    "list-min-value",
    "list-max-value",
    // String and value conversion
    "string-byte-length",
    "string-char-length",
    "value->string",
    "string->value?",
    "constantly",
    // REPL commands
    "version",
    "who",
    "help",
    "display",
    "write",
    "newline",
    // File I/O primitives (for forum support)
    "file-exists?",
    "file-directory?",
    "read-file",
    "read-file-bytes",
    "write-file",
    "write-file-bytes",
    "append-file",
    "mkdir",
    "mkdir-p",
    "delete-file",
    "directory-list",
    "current-time-seconds",
    "current-time-millis",
    "current-timestamp",
    // Forum commands
    "forum-channels",
    "forum-digest",
    "channels",
    "digest",
    // Session commands
    "hi",
    "who",
    "bye",
    // Chat and browse
    "chat",
    "browse",
    // Forum posting
    "msg",
    "digest-posts",
];

/// Create an environment with all primitives bound as first-class values.
/// This allows primitives like + to be passed directly to higher-order functions.
pub fn primitives_env() -> EnvRef {
    let env = Env::new();
    for &name in PRIMITIVE_NAMES {
        Env::insert(&env, name.to_string(), Value::Primitive(name.to_string()));
    }
    env
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prelude_assembles() {
        let source = prelude_source();
        assert!(source.len() > 10000, "prelude should be substantial");
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
}
