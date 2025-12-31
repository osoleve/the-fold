use std::fmt;

use crate::fabric::{CaseArm, Expr, SpannedExpr, Symbol, Value};
use crate::tools::fold_parse::{NumberLit, Sexp, Span, Spanned};
use num_traits::Zero;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LowerError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message, self.span)
    }
}

impl std::error::Error for LowerError {}

/// Lower a spanned S-expression to a spanned expression, preserving source locations.
pub fn lower_expr(expr: &Spanned<Sexp>) -> Result<SpannedExpr, LowerError> {
    let span = Some(expr.span.clone());
    let inner = match &expr.value {
        Sexp::Number(NumberLit::Integer(n)) => Expr::Value(Value::Number(*n)),
        Sexp::Number(NumberLit::Float(n)) => Expr::Value(Value::Float(*n)),
        Sexp::Number(NumberLit::Rational(numer, denom)) => {
            // Parse numerator and denominator as BigInts, create BigRational
            use num_bigint::BigInt;
            use num_rational::BigRational;
            let numer_bigint = numer.parse::<BigInt>().map_err(|e| LowerError {
                message: format!("invalid rational numerator: {}", e),
                span: expr.span.clone(),
            })?;
            let denom_bigint = denom.parse::<BigInt>().map_err(|e| LowerError {
                message: format!("invalid rational denominator: {}", e),
                span: expr.span.clone(),
            })?;
            if denom_bigint.is_zero() {
                return Err(LowerError {
                    message: "rational denominator cannot be zero".to_string(),
                    span: expr.span.clone(),
                });
            }
            Expr::Value(Value::BigRational(BigRational::new(
                numer_bigint,
                denom_bigint,
            )))
        }
        Sexp::String(s) => Expr::Value(Value::String(s.clone())),
        Sexp::Bool(b) => Expr::Value(Value::Bool(*b)),
        Sexp::Char(c) => Expr::Value(Value::Char(*c)),
        Sexp::Bytevector(bytes) => Expr::Value(Value::Bytevector(bytes.clone())),
        Sexp::Vector(items) => {
            // Lower each item in the vector and create a vec-make call
            let lowered: Result<Vec<_>, _> = items.iter().map(lower_expr).collect();
            let lowered_exprs = lowered?;
            return Ok(SpannedExpr::new(
                Expr::Prim {
                    op: "vec-make".into(),
                    args: lowered_exprs,
                },
                span,
            ));
        }
        Sexp::Symbol(sym) => Expr::Var(Symbol::intern(sym)),
        Sexp::List(items) => return lower_list(expr, items),
        Sexp::DottedList { .. } => {
            return Err(LowerError {
                message: "dotted list in expression position (use quote for data)".to_string(),
                span: expr.span.clone(),
            });
        }
    };
    Ok(SpannedExpr::new(inner, span))
}

pub fn lower_program(exprs: &[Spanned<Sexp>]) -> Result<Vec<SpannedExpr>, LowerError> {
    exprs.iter().map(lower_expr).collect()
}

/// Check if a symbol is a known primitive operator.
fn is_builtin_prim(name: &str) -> bool {
    matches!(
        name,
        // Arithmetic
        "+" | "-" | "*" | "/" | "add" | "sub" | "mul" | "div" | "mod"
            | "quotient" | "remainder"
            | "abs" | "neg" | "sqrt" | "expt" | "pow" | "floor" | "ceiling" | "round"
            | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "log" | "exp" | "min" | "max"
            | "gcd" | "lcm" | "clamp" | "between?" | "sign" | "divmod"
            | "int->float" | "float->int" | "truncate"
            | "inc" | "dec" | "succ" | "pred"
            | "sq" | "cube" | "double" | "halve" | "negate" | "zero" | "one"
            | "scale" | "denormalize" | "lerp" | "inverse-lerp" | "smoothstep"
            | "saturation-add" | "percent-of" | "percent-change" | "round-to"
            // Rational operations
            | "make-rational" | "rational" | "rational?" | "rational-zero?"
            | "rational-positive?" | "rational-negative?" | "rational-integer?"
            | "rational-numerator" | "rational-denominator"
            | "rational-add" | "rational-sub" | "rational-mul" | "rational-div"
            | "rational-abs" | "rational-neg" | "rational-recip"
            | "rational-floor" | "rational-ceiling" | "rational-round" | "rational-truncate"
            | "rational->float" | "rational<?" | "rational<=?" | "rational=?" | "rational>=?" | "rational>?"
            // Comparison
            | "<" | ">" | "<=" | ">=" | "=" | "eq?" | "lt?" | "gt?" | "le?" | "ge?"
            // Type checks and predicates
            | "number?" | "integer?" | "string?" | "list?" | "pair?" | "null?" | "boolean?"
            | "symbol?" | "procedure?" | "char?" | "vector?" | "bytevector?"
            | "zero?" | "positive?" | "negative?" | "even?" | "odd?"
            | "atom?" | "atomic?" | "not-null?" | "proper-list?" | "pair-list?"
            // Symbol operations
            | "symbol-upcase" | "symbol-downcase" | "symbol-length" | "symbol-concat"
            // Bytevector operations
            | "bytevector-length" | "bytevector-ref" | "bytevector->string" | "string->bytevector"
            // Association list operations
            | "alist-get" | "alist-set" | "alist-keys" | "alist-values"
            // List operations
            | "car" | "cdr" | "cons" | "set-car!" | "set-cdr!"
            | "list" | "append" | "length" | "reverse" | "memq"
            | "list-ref" | "list->string" | "list->vec" | "take" | "drop" | "last" | "init" | "flatten" | "range"
            | "list-copy" | "make-list" | "first" | "rest" | "second" | "third" | "head" | "tail"
            | "repeat" | "sum" | "product" | "sum-of" | "max-of" | "min-of"
            | "sort" | "unique" | "find-index" | "index-of" | "count"
            | "find" | "contains?" | "min-list" | "max-list"
            // Note: member and position removed - implemented in prelude with Scheme-standard argument order
            | "nth" | "zip" | "unzip" | "rotate"
            | "list->set" | "interleave" | "list->pairs"
            | "list->symbols" | "list->numbers"
            | "list-indices" | "list-reverse-indices" | "list-length-of" | "list-split"
            | "list-repeat-element" | "list-cycle" | "list-intersperse"
            | "list-map-indexed" | "list-enumerate"
            // Note: filter removed - requires closure evaluation, handled at evaluator level
            // String operations
            | "string-append" | "string-length" | "string-ref" | "substring"
            | "string->list" | "string->number" | "string->symbol" | "string->utf8"
            | "string=?" | "string<?" | "string>?"
            | "string-upcase" | "string-downcase" | "upcase" | "upcase-string"
            | "downcase" | "downcase-string" | "string-split"
            | "string-trim" | "string-ltrim" | "string-rtrim" | "string-pad"
            | "string-starts-with?" | "string-ends-with?" | "string-contains?"
            | "string-reverse" | "string-repeat" | "string-replace"
            | "string-index" | "string-rindex"
            | "pad-left" | "pad-right" | "string-center"
            | "char-code" | "code-char" | "chars"
            | "string-concat"
            | "string-take" | "string-drop" | "starts-with?" | "ends-with?" | "substring?"
            | "string-words" | "string-lines" | "string-join" | "string-replace-all"
            | "string-trim-start" | "string-trim-end" | "string-trim-both"
            | "string-ascii?" | "string-numeric?" | "string-alphabetic?" | "string-alphanumeric?"
            | "string-whitespace?" | "string-empty?"
            | "format" | "number->string" | "symbol->string"
            // Vector operations
            | "vec-length" | "vec-ref" | "vec-make" | "vec->list"
            | "vec-copy" | "vec-slice" | "vec-reverse" | "vec-sort"
            | "vec-contains?" | "vec-index-of"
            // Type introspection
            | "type-of" | "is-number?" | "is-string?" | "is-vector?" | "is-list?"
            // Value comparison
            | "equals?" | "not-equals?" | "identical?" | "hash-value"
            // Block operations
            | "make-block" | "block" | "block?" | "block-tag" | "block-ref" | "block->bytes"
            | "block-payload" | "block-refs"
            // Hash/CAS
            | "hash-block" | "sha256" | "fetch" | "store!" | "pin!" | "pinned?"
            // Byte operations
            | "bv-make" | "bv-length" | "bv-ref" | "bv-slice" | "bv-concat" | "bv-copy"
            | "bytes->block"
            // Character operations
            | "integer->char" | "char->integer" | "char=?" | "char<?"
            // Logic
            | "and" | "or" | "not"
            // Logical operations
            | "xor" | "gensym"
            // Conditional and logical utilities
            | "when" | "unless" | "implies" | "iff" | "nand" | "nor" | "if"
            | "cond-value" | "coalesce" | "default-value" | "both?" | "either?"
            // Utilities
            | "identity" | "error" | "deep-equal?" | "list-every?" | "apply"
            | "assq" | "assoc"
            // List analysis utilities
            | "list-count" | "list-sum" | "list-avg" | "list-min-value" | "list-max-value"
            // String and value conversion
            | "string-byte-length" | "string-char-length" | "value->string" | "string->value?"
            // Note: constantly removed - implemented in prelude as HOF
            // REPL commands
            | "version" | "who" | "help" | "display" | "write" | "newline"
    )
}

fn lower_list(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.is_empty() {
        return Ok(SpannedExpr::new(Expr::Value(Value::Nil), span));
    }
    if let Some(head_symbol) = symbol_name(&items[0]) {
        match head_symbol.as_str() {
            "quote" => return lower_quote(list_expr, items),
            "quasiquote" => return lower_quasiquote(list_expr, items),
            "fn" | "lambda" => return lower_fn(list_expr, items),
            "let" => return lower_let(list_expr, items),
            "let*" => return lower_let_star(list_expr, items),
            "fix" => return lower_fix(list_expr, items),
            "if" => return lower_if(list_expr, items),
            "case" => return lower_case(list_expr, items),
            "prim" => return lower_prim(list_expr, items),
            "call" => return lower_call(list_expr, items),
            "load" => return lower_load(list_expr, items),
            "define" => return lower_define(list_expr, items),
            "define-syntax" => {
                // Macros not supported - skip as no-op
                let span = Some(list_expr.span.clone());
                return Ok(SpannedExpr::new(Expr::Value(Value::Nil), span));
            }
            "begin" => return lower_begin(list_expr, items),
            "cond" => return lower_cond(list_expr, items),
            "and" => return lower_and(list_expr, items),
            "or" => return lower_or(list_expr, items),
            _ => {
                // Check if this is a builtin primitive that should be lowered to a prim call
                if is_builtin_prim(&head_symbol) {
                    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
                    for arg in &items[1..] {
                        args.push(lower_expr(arg)?);
                    }
                    return Ok(SpannedExpr::new(
                        Expr::Prim {
                            op: Symbol::intern(&head_symbol),
                            args,
                        },
                        span,
                    ));
                }
            }
        }
    }

    let func = lower_expr(&items[0])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for arg in &items[1..] {
        args.push(lower_expr(arg)?);
    }
    Ok(SpannedExpr::new(
        Expr::Call {
            func: Box::new(func),
            args,
        },
        span,
    ))
}

fn lower_quote(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 2 {
        return Err(error(list_expr, "quote expects 1 argument"));
    }
    let datum = value_from_spanned(&items[1])?;
    Ok(SpannedExpr::new(Expr::Quote(datum), span))
}

/// Expand quasiquote - handles unquote (,) and unquote-splicing (,@)
fn lower_quasiquote(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 2 {
        return Err(error(list_expr, "quasiquote expects 1 argument"));
    }
    let inner = expand_quasiquote(&items[1])?;
    Ok(SpannedExpr::new(inner, span))
}

/// Recursively expand a quasiquoted expression
fn expand_quasiquote(expr: &Spanned<Sexp>) -> Result<Expr, LowerError> {
    match &expr.value {
        Sexp::List(items) if !items.is_empty() => {
            // Check for (unquote x)
            if let Some(head) = symbol_name(&items[0])
                && head == "unquote"
            {
                if items.len() != 2 {
                    return Err(error(expr, "unquote expects 1 argument"));
                }
                // lower_expr returns SpannedExpr, extract the inner Expr
                return Ok(lower_expr(&items[1])?.expr);
            }
            // It's a list - expand each element
            expand_quasiquote_list(items)
        }
        Sexp::DottedList { heads, tail } => {
            // Expand dotted list in quasiquote context
            expand_quasiquote_dotted_list(heads, tail)
        }
        // Non-list or empty list - just quote it
        _ => {
            let datum = value_from_spanned(expr)?;
            Ok(Expr::Quote(datum))
        }
    }
}

/// Expand a quasiquoted list, handling unquote-splicing
fn expand_quasiquote_list(items: &[Spanned<Sexp>]) -> Result<Expr, LowerError> {
    // Check if any element uses unquote-splicing
    let has_splicing = items.iter().any(|item| {
        if let Sexp::List(inner) = &item.value
            && !inner.is_empty()
            && let Some(head) = symbol_name(&inner[0])
        {
            return head == "unquote-splicing";
        }
        false
    });

    if has_splicing {
        // Need to use append for splicing
        let mut append_args: Vec<SpannedExpr> = Vec::new();
        for item in items {
            if let Sexp::List(inner) = &item.value
                && !inner.is_empty()
                && let Some(head) = symbol_name(&inner[0])
                && head == "unquote-splicing"
            {
                if inner.len() != 2 {
                    return Err(error(item, "unquote-splicing expects 1 argument"));
                }
                // Splice: the value should be a list to splice in
                append_args.push(lower_expr(&inner[1])?);
                continue;
            }
            // Regular element - wrap in (list <expanded-element>)
            let expanded = expand_quasiquote(item)?;
            append_args.push(SpannedExpr::new(
                Expr::Prim {
                    op: Symbol::intern("list"),
                    args: vec![SpannedExpr::unspanned(expanded)],
                },
                Some(item.span.clone()),
            ));
        }
        Ok(Expr::Prim {
            op: Symbol::intern("append"),
            args: append_args,
        })
    } else {
        // No splicing - use (list ...)
        let mut list_args: Vec<SpannedExpr> = Vec::new();
        for item in items {
            list_args.push(SpannedExpr::new(
                expand_quasiquote(item)?,
                Some(item.span.clone()),
            ));
        }
        Ok(Expr::Prim {
            op: Symbol::intern("list"),
            args: list_args,
        })
    }
}

/// Expand a quasiquoted dotted list like `(a b . ,c)
fn expand_quasiquote_dotted_list(
    heads: &[Spanned<Sexp>],
    tail: &Spanned<Sexp>,
) -> Result<Expr, LowerError> {
    // For a dotted list, we need to use cons to build the structure
    // `(a b . c) becomes (cons (expand a) (cons (expand b) (expand c)))
    let tail_expr = expand_quasiquote(tail)?;
    let mut result = tail_expr;

    for head in heads.iter().rev() {
        let head_expr = expand_quasiquote(head)?;
        result = Expr::Prim {
            op: "cons".to_string(),
            args: vec![
                SpannedExpr::new(head_expr, Some(head.span.clone())),
                SpannedExpr::unspanned(result),
            ],
        };
    }

    Ok(result)
}

fn lower_fn(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 3 {
        return Err(error(list_expr, "fn expects (fn (params...) body)"));
    }
    let params = match &items[1].value {
        Sexp::List(list) => list
            .iter()
            .map(|param| match &param.value {
                Sexp::Symbol(name) => Ok(Symbol::intern(name)),
                _ => Err(error(param, "fn params must be symbols")),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(error(&items[1], "fn expects a parameter list")),
    };
    let body = lower_expr(&items[2])?;
    Ok(SpannedExpr::new(
        Expr::Fn {
            params,
            body: Box::new(body),
        },
        span,
    ))
}

fn lower_let(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());

    // Check for named let: (let name ((var init) ...) body)
    if items.len() >= 4
        && let Sexp::Symbol(loop_name) = &items[1].value
    {
        // Named let: (let loop ((x 1) (y 2)) body)
        // Transforms to: ((fix loop (lambda (x y) body)) 1 2)
        let bindings_list = match &items[2].value {
            Sexp::List(list) => list,
            _ => return Err(error(&items[2], "named let expects bindings list")),
        };

        let mut params: Vec<String> = Vec::new();
        let mut init_exprs: Vec<SpannedExpr> = Vec::new();

        for binding in bindings_list {
            match &binding.value {
                Sexp::List(pair) if pair.len() == 2 => {
                    let name = match &pair[0].value {
                        Sexp::Symbol(sym) => sym.clone(),
                        _ => return Err(error(&pair[0], "binding name must be a symbol")),
                    };
                    params.push(name);
                    init_exprs.push(lower_expr(&pair[1])?);
                }
                _ => return Err(error(binding, "binding must be (name expr)")),
            }
        }

        // Handle body - could be multiple expressions
        let body = if items.len() == 4 {
            lower_expr(&items[3])?
        } else {
            lower_begin(list_expr, &items[3..])?
        };

        // Build: ((fix loop (lambda (params...) body)) init_exprs...)
        let lambda = SpannedExpr::new(
            Expr::Fn {
                params,
                body: Box::new(body),
            },
            span.clone(),
        );

        let fixed = SpannedExpr::new(
            Expr::Fix {
                name: loop_name.clone(),
                value: Box::new(lambda),
            },
            span.clone(),
        );

        return Ok(SpannedExpr::new(
            Expr::Call {
                func: Box::new(fixed),
                args: init_exprs,
            },
            span,
        ));
    }

    // Regular let: (let ((name expr) ...) body)
    if items.len() != 3 {
        return Err(error(list_expr, "let expects (let ((name expr) ...) body)"));
    }
    let bindings_list = match &items[1].value {
        Sexp::List(list) => list,
        _ => return Err(error(&items[1], "let expects a bindings list")),
    };
    let mut bindings = Vec::with_capacity(bindings_list.len());
    for binding in bindings_list {
        match &binding.value {
            Sexp::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].value {
                    Sexp::Symbol(sym) => Symbol::intern(sym),
                    _ => return Err(error(&pair[0], "let binding name must be a symbol")),
                };
                let expr = lower_expr(&pair[1])?;
                bindings.push((name, expr));
            }
            _ => return Err(error(binding, "let binding must be (name expr)")),
        }
    }
    let body = lower_expr(&items[2])?;
    Ok(SpannedExpr::new(
        Expr::Let {
            bindings,
            body: Box::new(body),
        },
        span,
    ))
}

/// Transform let* into nested let expressions
/// (let* ((a 1) (b (+ a 1))) body) => (let ((a 1)) (let ((b (+ a 1))) body))
fn lower_let_star(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 3 {
        return Err(error(
            list_expr,
            "let* expects (let* ((name expr) ...) body)",
        ));
    }
    let bindings_list = match &items[1].value {
        Sexp::List(list) => list,
        _ => return Err(error(&items[1], "let* expects a bindings list")),
    };

    // Parse all bindings
    let mut bindings: Vec<(Symbol, SpannedExpr)> = Vec::with_capacity(bindings_list.len());
    for binding in bindings_list {
        match &binding.value {
            Sexp::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].value {
                    Sexp::Symbol(sym) => Symbol::intern(sym),
                    _ => return Err(error(&pair[0], "let* binding name must be a symbol")),
                };
                let expr = lower_expr(&pair[1])?;
                bindings.push((name, expr));
            }
            _ => return Err(error(binding, "let* binding must be (name expr)")),
        }
    }

    // If no bindings, just return the body
    if bindings.is_empty() {
        return lower_expr(&items[2]);
    }

    // Build nested let expressions from inside out
    let mut body = lower_expr(&items[2])?;
    for (name, expr) in bindings.into_iter().rev() {
        body = SpannedExpr::new(
            Expr::Let {
                bindings: vec![(name, expr)],
                body: Box::new(body),
            },
            span.clone(),
        );
    }
    Ok(body)
}

fn lower_fix(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 3 {
        return Err(error(list_expr, "fix expects (fix name expr)"));
    }
    let name = match &items[1].value {
        Sexp::Symbol(sym) => Symbol::intern(sym),
        _ => return Err(error(&items[1], "fix name must be a symbol")),
    };
    let value = lower_expr(&items[2])?;
    Ok(SpannedExpr::new(
        Expr::Fix {
            name,
            value: Box::new(value),
        },
        span,
    ))
}

fn lower_if(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 4 {
        return Err(error(list_expr, "if expects (if test then else)"));
    }
    Ok(SpannedExpr::new(
        Expr::If {
            test: Box::new(lower_expr(&items[1])?),
            then_branch: Box::new(lower_expr(&items[2])?),
            else_branch: Box::new(lower_expr(&items[3])?),
        },
        span,
    ))
}

fn lower_case(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() < 2 {
        return Err(error(
            list_expr,
            "case expects (case expr (pattern body) ...)",
        ));
    }
    let scrutinee = lower_expr(&items[1])?;
    let mut arms = Vec::with_capacity(items.len().saturating_sub(2));
    let mut else_body = None;

    for clause in &items[2..] {
        let clause_items = match &clause.value {
            Sexp::List(list) if list.len() == 2 => list,
            _ => {
                return Err(error(
                    clause,
                    "case clause must be (pattern body) or (else body)",
                ));
            }
        };

        // Check for (else body) clause
        if let Sexp::Symbol(sym) = &clause_items[0].value
            && sym == "else"
        {
            if else_body.is_some() {
                return Err(error(clause, "case can only have one else clause"));
            }
            else_body = Some(Box::new(lower_expr(&clause_items[1])?));
            continue;
        }

        // Regular pattern clause: ((tag vars...) body)
        let pattern_items = match &clause_items[0].value {
            Sexp::List(list) if !list.is_empty() => list,
            _ => {
                return Err(error(
                    &clause_items[0],
                    "case pattern must be (tag vars...) or else",
                ));
            }
        };
        let tag = match &pattern_items[0].value {
            Sexp::Symbol(sym) => Symbol::intern(sym),
            _ => return Err(error(&pattern_items[0], "case tag must be a symbol")),
        };
        let mut vars = Vec::with_capacity(pattern_items.len().saturating_sub(1));
        for var in &pattern_items[1..] {
            match &var.value {
                Sexp::Symbol(sym) => vars.push(Symbol::intern(sym)),
                _ => return Err(error(var, "case pattern vars must be symbols")),
            }
        }
        let body = lower_expr(&clause_items[1])?;
        arms.push(CaseArm::new(tag, vars, body));
    }
    Ok(SpannedExpr::new(
        Expr::Case {
            expr: Box::new(scrutinee),
            arms,
            else_body,
        },
        span,
    ))
}

fn lower_prim(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() < 2 {
        return Err(error(list_expr, "prim expects (prim op args...)"));
    }
    let op = parse_prim_op(&items[1])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(2));
    for arg in &items[2..] {
        args.push(lower_expr(arg)?);
    }
    Ok(SpannedExpr::new(Expr::Prim { op, args }, span))
}

fn lower_call(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() < 2 {
        return Err(error(list_expr, "call expects (call func args...)"));
    }
    let func = lower_expr(&items[1])?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(2));
    for arg in &items[2..] {
        args.push(lower_expr(arg)?);
    }
    Ok(SpannedExpr::new(
        Expr::Call {
            func: Box::new(func),
            args,
        },
        span,
    ))
}

fn lower_load(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() != 2 {
        return Err(error(list_expr, "load expects (load path)"));
    }
    let path = lower_expr(&items[1])?;
    Ok(SpannedExpr::new(
        Expr::Load {
            path: Box::new(path),
        },
        span,
    ))
}

fn parse_prim_op(op: &Spanned<Sexp>) -> Result<Symbol, LowerError> {
    match &op.value {
        Sexp::Symbol(sym) => Ok(Symbol::intern(sym)),
        Sexp::List(items) if items.len() == 2 => {
            if let Some(head) = symbol_name(&items[0])
                && head == "quote"
            {
                if let Sexp::Symbol(sym) = &items[1].value {
                    return Ok(Symbol::intern(sym));
                }
                return Err(error(&items[1], "prim op must be a symbol"));
            }
            Err(error(op, "prim op must be a symbol or quoted symbol"))
        }
        _ => Err(error(op, "prim op must be a symbol or quoted symbol")),
    }
}

fn value_from_spanned(expr: &Spanned<Sexp>) -> Result<Value, LowerError> {
    match &expr.value {
        Sexp::Number(NumberLit::Integer(n)) => Ok(Value::Number(*n)),
        Sexp::Number(NumberLit::Float(n)) => Ok(Value::Float(*n)),
        Sexp::Number(NumberLit::Rational(numer, denom)) => {
            use num_bigint::BigInt;
            use num_rational::BigRational;
            let numer_bigint = numer.parse::<BigInt>().map_err(|e| LowerError {
                message: format!("invalid rational numerator: {}", e),
                span: expr.span.clone(),
            })?;
            let denom_bigint = denom.parse::<BigInt>().map_err(|e| LowerError {
                message: format!("invalid rational denominator: {}", e),
                span: expr.span.clone(),
            })?;
            if denom_bigint.is_zero() {
                return Err(LowerError {
                    message: "rational denominator cannot be zero".to_string(),
                    span: expr.span.clone(),
                });
            }
            Ok(Value::BigRational(BigRational::new(
                numer_bigint,
                denom_bigint,
            )))
        }
        Sexp::String(s) => Ok(Value::String(s.clone())),
        Sexp::Char(c) => Ok(Value::Char(*c)),
        Sexp::Bytevector(bytes) => Ok(Value::Bytevector(bytes.clone())),
        Sexp::Vector(_items) => {
            // Vectors can't be pure literals since they contain expressions
            Err(LowerError {
                message: "vectors are not literals".to_string(),
                span: expr.span.clone(),
            })
        }
        Sexp::Symbol(sym) => Ok(Value::Symbol(Symbol::intern(sym))),
        Sexp::Bool(b) => Ok(Value::Bool(*b)),
        Sexp::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(value_from_spanned(item)?);
            }
            Ok(list_from_values(&values))
        }
        Sexp::DottedList { heads, tail } => {
            // Convert heads to values
            let head_values: Result<Vec<_>, _> = heads.iter().map(value_from_spanned).collect();
            let head_values = head_values?;
            let tail_value = value_from_spanned(tail)?;
            // Build the improper list from the end
            let mut result = tail_value;
            for v in head_values.into_iter().rev() {
                result = Value::Pair(Box::new(v), Box::new(result));
            }
            Ok(result)
        }
    }
}

/// Lower (define name value) or (define (name args...) body)
fn lower_define(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());
    if items.len() < 3 {
        return Err(error(
            list_expr,
            "define expects (define name value) or (define (name args...) body)",
        ));
    }

    match &items[1].value {
        // Simple define: (define x 42)
        Sexp::Symbol(name) => {
            if items.len() != 3 {
                return Err(error(list_expr, "define expects (define name value)"));
            }
            let value = lower_expr(&items[2])?;
            Ok(SpannedExpr::new(
                Expr::Define {
                    name: name.clone(),
                    value: Box::new(value),
                },
                span,
            ))
        }
        // Function define: (define (name args...) body...)
        Sexp::List(name_and_params) if !name_and_params.is_empty() => {
            let name = match &name_and_params[0].value {
                Sexp::Symbol(s) => s.clone(),
                _ => return Err(error(&name_and_params[0], "function name must be a symbol")),
            };
            let params: Result<Vec<_>, _> = name_and_params[1..]
                .iter()
                .map(|p| match &p.value {
                    Sexp::Symbol(s) => Ok(s.clone()),
                    _ => Err(error(p, "parameter must be a symbol")),
                })
                .collect();
            let params = params?;

            // Handle body - if multiple expressions, wrap in begin
            let body = if items.len() == 3 {
                lower_expr(&items[2])?
            } else {
                // Multiple body expressions - lower each and make a sequence
                lower_begin(list_expr, &items[2..])?
            };

            let lambda = SpannedExpr::new(
                Expr::Fn {
                    params,
                    body: Box::new(body),
                },
                span.clone(),
            );

            Ok(SpannedExpr::new(
                Expr::Define {
                    name,
                    value: Box::new(lambda),
                },
                span,
            ))
        }
        _ => Err(error(&items[1], "define expects symbol or (name args...)")),
    }
}

/// Lower (begin expr...) to a sequence of expressions
fn lower_begin(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());

    // Skip "begin" if present at the start
    let exprs = if !items.is_empty() && symbol_name(&items[0]).as_deref() == Some("begin") {
        &items[1..]
    } else {
        items
    };

    if exprs.is_empty() {
        // Empty begin returns nil
        return Ok(SpannedExpr::new(Expr::Value(Value::Nil), span));
    }

    if exprs.len() == 1 {
        return lower_expr(&exprs[0]);
    }

    // Convert to nested let bindings: (let ((_ e1)) (let ((_ e2)) e3))
    let mut result = lower_expr(exprs.last().unwrap())?;
    for (i, expr) in exprs.iter().rev().skip(1).enumerate() {
        let lowered = lower_expr(expr)?;
        result = SpannedExpr::new(
            Expr::Let {
                bindings: vec![(format!("#%begin-{}", i), lowered)],
                body: Box::new(result),
            },
            span.clone(),
        );
    }

    Ok(result)
}

/// Lower (cond (test expr)... [(else expr)])
fn lower_cond(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());

    // Skip "cond"
    let clauses = &items[1..];

    if clauses.is_empty() {
        return Ok(SpannedExpr::new(Expr::Value(Value::Nil), span));
    }

    // Build nested if expressions from bottom up
    let mut result: Option<SpannedExpr> = None;

    for clause in clauses.iter().rev() {
        let clause_items = match &clause.value {
            Sexp::List(items) => items,
            _ => return Err(error(clause, "cond clause must be a list")),
        };

        if clause_items.is_empty() {
            return Err(error(clause, "cond clause cannot be empty"));
        }

        // Check for else clause
        if let Some("else") = symbol_name(&clause_items[0]).as_deref() {
            if clause_items.len() < 2 {
                return Err(error(clause, "else clause needs a body"));
            }
            // Lower the else body
            result = Some(if clause_items.len() == 2 {
                lower_expr(&clause_items[1])?
            } else {
                lower_begin(clause, &clause_items[1..])?
            });
        } else {
            // Regular clause: (test body...)
            let test = lower_expr(&clause_items[0])?;
            let body = if clause_items.len() == 1 {
                // (cond (test)) returns test if true
                test.clone()
            } else if clause_items.len() == 2 {
                lower_expr(&clause_items[1])?
            } else {
                lower_begin(clause, &clause_items[1..])?
            };

            let else_branch =
                result.unwrap_or_else(|| SpannedExpr::new(Expr::Value(Value::Nil), None));

            result = Some(SpannedExpr::new(
                Expr::If {
                    test: Box::new(test),
                    then_branch: Box::new(body),
                    else_branch: Box::new(else_branch),
                },
                span.clone(),
            ));
        }
    }

    Ok(result.unwrap_or_else(|| SpannedExpr::new(Expr::Value(Value::Nil), span)))
}

/// Lower (and expr...) to nested if expressions
fn lower_and(
    list_expr: &Spanned<Sexp>,
    items: &[Spanned<Sexp>],
) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());

    // Skip "and"
    let exprs = &items[1..];

    if exprs.is_empty() {
        return Ok(SpannedExpr::new(Expr::Value(Value::Bool(true)), span));
    }

    if exprs.len() == 1 {
        return lower_expr(&exprs[0]);
    }

    // Build nested if: (if e1 (if e2 e3 #f) #f)
    let mut result = lower_expr(exprs.last().unwrap())?;
    for expr in exprs.iter().rev().skip(1) {
        let test = lower_expr(expr)?;
        result = SpannedExpr::new(
            Expr::If {
                test: Box::new(test),
                then_branch: Box::new(result),
                else_branch: Box::new(SpannedExpr::new(Expr::Value(Value::Bool(false)), None)),
            },
            span.clone(),
        );
    }

    Ok(result)
}

/// Lower (or expr...) to nested if expressions
fn lower_or(list_expr: &Spanned<Sexp>, items: &[Spanned<Sexp>]) -> Result<SpannedExpr, LowerError> {
    let span = Some(list_expr.span.clone());

    // Skip "or"
    let exprs = &items[1..];

    if exprs.is_empty() {
        return Ok(SpannedExpr::new(Expr::Value(Value::Bool(false)), span));
    }

    if exprs.len() == 1 {
        return lower_expr(&exprs[0]);
    }

    // Build nested if: (if e1 e1 (if e2 e2 e3))
    // But we need to avoid evaluating e1 twice, so use a let
    // Actually, for simplicity, we'll just evaluate twice for now
    // TODO: Use proper short-circuit with let bindings
    let mut result = lower_expr(exprs.last().unwrap())?;
    for expr in exprs.iter().rev().skip(1) {
        let test = lower_expr(expr)?;
        result = SpannedExpr::new(
            Expr::If {
                test: Box::new(test.clone()),
                then_branch: Box::new(test),
                else_branch: Box::new(result),
            },
            span.clone(),
        );
    }

    Ok(result)
}

fn list_from_values(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}

fn symbol_name(expr: &Spanned<Sexp>) -> Option<String> {
    match &expr.value {
        Sexp::Symbol(sym) => Some(sym.clone()),
        _ => None,
    }
}

fn error(expr: &Spanned<Sexp>, message: &str) -> LowerError {
    error_at(expr, message)
}

fn error_at(expr: &Spanned<Sexp>, message: &str) -> LowerError {
    LowerError {
        message: message.to_string(),
        span: expr.span.clone(),
    }
}
