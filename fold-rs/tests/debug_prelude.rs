use fold_rs::tools::fold_parse::Sexp;
use fold_rs::tools::parse_fold_expr;
use std::fs;

fn assemble_prelude() -> String {
    let prelude_dir = std::path::Path::new("src/tools/prelude");
    let files = [
        "core.ss",
        "function.ss",
        "list.ss",
        "list-core.ss",
        "list-search.ss",
        "list-transform.ss",
        "list-partition.ss",
        "equality.ss",
        "maybe.ss",
        "either.ss",
        "numeric.ss",
        "numeric-core.ss",
        "numeric-stats.ss",
        "numeric-sequence.ss",
        "comparison.ss",
        "string.ss",
        "string-core.ss",
        "string-format.ss",
        "string-search.ss",
        "collection.ss",
        "collection-alist.ss",
        "collection-dict.ss",
        "collection-set.ss",
        "collection-bag.ss",
        "control.ss",
        "validation.ss",
        "validation-core.ss",
        "validation-assert.ss",
        "stream.ss",
        "datastructures.ss",
        "graph.ss",
        "graph-core.ss",
        "graph-traversal.ss",
        "graph-algorithm.ss",
        "tree.ss",
        "stats.ss",
        "combinatorics.ss",
        "list-ext.ss",
        "string-ext.ss",
        "graph-ext.ss",
        "alist-ext.ss",
        "numeric-ext.ss",
        "set-ext.ss",
        "logic-ext.ss",
        "monad.ss",
        "monad-core.ss",
        "missing.ss",
        "path.ss",
        "encoding.ss",
        "compat.ss",
    ];

    let mut source = String::with_capacity(256 * 1024);
    source.push_str("(let*\n  (");

    for f in files {
        let content = fs::read_to_string(prelude_dir.join(f)).unwrap_or_default();
        source.push_str(&content);
        source.push('\n');
    }

    let exports = fs::read_to_string(prelude_dir.join("exports.ss")).unwrap_or_default();
    source.push_str(")\n  ");
    source.push_str(&exports);
    source.push_str("\n)");

    source
}

#[test]
fn find_all_bad_lets() {
    let source = assemble_prelude();
    let parsed = parse_fold_expr(&source, Some("prelude")).expect("parse");

    fn find_bad_lets(sexp: &fold_rs::tools::fold_parse::Spanned<Sexp>, path: &str) {
        if let Sexp::List(items) = &sexp.value {
            if let Some(first) = items.first()
                && let Sexp::Symbol(s) = &first.value
                && s == "let"
                && items.len() != 3
            {
                println!("BAD LET at {} ({} items): {}", sexp.span, items.len(), path);
            }
            for (i, item) in items.iter().enumerate() {
                find_bad_lets(item, &format!("{}/{}", path, i));
            }
        }
    }

    find_bad_lets(&parsed, "root");
    println!("Done searching.");
}
