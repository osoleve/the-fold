//! S-expression alpha-normalization via de Bruijn indices
//!
//! Converts named variables to positional indices, ensuring that
//! alpha-equivalent expressions produce identical canonical forms.
//!
//! ```text
//! (fn (x) x)           -> (fn (dv 0))
//! (fn (x) (fn (y) x))  -> (fn (fn (dv 1)))
//! (fn (x) (fn (y) y))  -> (fn (fn (dv 0)))
//! ```
//!
//! Binder forms recognized:
//!   (fn (var) body)        - single-argument function
//!   (let ((var val)) body) - single binding let
//!   (fix (f) body)         - recursive binding
//!
//! (dv n) represents a de Bruijn variable with index n.

use crate::fabric::{symbol::Symbol, value::Value};

/// An environment mapping symbols to de Bruijn indices.
/// The innermost binding is at index 0.
#[derive(Clone, Default)]
pub struct NormEnv {
    bindings: Vec<Symbol>,
}

impl NormEnv {
    /// Create an empty environment.
    pub fn new() -> Self {
        Self { bindings: vec![] }
    }

    /// Extend the environment with a new binding.
    pub fn extend(&self, sym: Symbol) -> Self {
        let mut new_bindings = vec![sym];
        new_bindings.extend(self.bindings.iter().cloned());
        Self {
            bindings: new_bindings,
        }
    }

    /// Look up a symbol, returning its de Bruijn index if found.
    pub fn lookup(&self, sym: &Symbol) -> Option<usize> {
        self.bindings.iter().position(|s| s == sym)
    }
}

/// Normalize an S-expression to de Bruijn form.
pub fn normalize(expr: &Value) -> Value {
    normalize_with_env(expr, &NormEnv::new())
}

/// Normalize an S-expression with the given environment.
pub fn normalize_with_env(expr: &Value, env: &NormEnv) -> Value {
    match expr {
        // Symbols: look up in environment
        Value::Symbol(sym) => {
            if let Some(idx) = env.lookup(sym) {
                // Bound variable -> de Bruijn index: (dv n)
                make_dv(idx)
            } else {
                // Free variable -> keep as symbol
                expr.clone()
            }
        }

        // Pairs: check for special forms
        Value::Pair(car, cdr) => {
            if let Value::Symbol(head) = car.as_ref() {
                match head.as_str().as_str() {
                    // (fn (var) body) -> (fn normalized-body)
                    "fn" => {
                        if let Some((var, body)) = parse_fn(cdr) {
                            let new_env = env.extend(var);
                            let norm_body = normalize_with_env(&body, &new_env);
                            make_fn_normalized(norm_body)
                        } else {
                            // Malformed fn, normalize as general list
                            normalize_list(expr, env)
                        }
                    }

                    // (let ((var val)) body) -> (let (normalized-val) normalized-body)
                    "let" => {
                        if let Some((var, val, body)) = parse_let(cdr) {
                            let norm_val = normalize_with_env(&val, env);
                            let new_env = env.extend(var);
                            let norm_body = normalize_with_env(&body, &new_env);
                            make_let_normalized(norm_val, norm_body)
                        } else {
                            normalize_list(expr, env)
                        }
                    }

                    // (fix (f) body) -> (fix normalized-body)
                    "fix" => {
                        if let Some((f, body)) = parse_fix(cdr) {
                            let new_env = env.extend(f);
                            let norm_body = normalize_with_env(&body, &new_env);
                            make_fix_normalized(norm_body)
                        } else {
                            normalize_list(expr, env)
                        }
                    }

                    // (quote datum) -> unchanged
                    "quote" => expr.clone(),

                    // Other forms: normalize as general list
                    _ => normalize_list(expr, env),
                }
            } else {
                // Not a symbol head, normalize as general list
                normalize_list(expr, env)
            }
        }

        // Non-list atoms pass through
        _ => expr.clone(),
    }
}

/// Parse (fn (var) body) - cdr is ((var) body)
fn parse_fn(cdr: &Value) -> Option<(Symbol, Value)> {
    // cdr should be ((var) body . nil)
    let (params, rest) = uncons(cdr)?;
    let (body, rest2) = uncons(&rest)?;
    if !is_nil(&rest2) {
        return None;
    }

    // params should be (var)
    let (var_val, param_rest) = uncons(&params)?;
    if !is_nil(&param_rest) {
        return None;
    }

    let var = match var_val {
        Value::Symbol(s) => s,
        _ => return None,
    };

    Some((var, body))
}

/// Parse (let ((var val)) body) - cdr is (((var val)) body)
fn parse_let(cdr: &Value) -> Option<(Symbol, Value, Value)> {
    // cdr should be (bindings body . nil)
    let (bindings, rest) = uncons(cdr)?;
    let (body, rest2) = uncons(&rest)?;
    if !is_nil(&rest2) {
        return None;
    }

    // bindings should be ((var val))
    let (binding, binding_rest) = uncons(&bindings)?;
    if !is_nil(&binding_rest) {
        return None;
    }

    // binding should be (var val)
    let (var_val, val_rest) = uncons(&binding)?;
    let (val, val_rest2) = uncons(&val_rest)?;
    if !is_nil(&val_rest2) {
        return None;
    }

    let var = match var_val {
        Value::Symbol(s) => s,
        _ => return None,
    };

    Some((var, val, body))
}

/// Parse (fix (f) body) - cdr is ((f) body)
fn parse_fix(cdr: &Value) -> Option<(Symbol, Value)> {
    // Same structure as fn
    parse_fn(cdr)
}

/// Helper: uncons a pair
fn uncons(val: &Value) -> Option<(Value, Value)> {
    match val {
        Value::Pair(car, cdr) => Some(((**car).clone(), (**cdr).clone())),
        _ => None,
    }
}

/// Helper: check if value is nil
fn is_nil(val: &Value) -> bool {
    matches!(val, Value::Nil)
}

/// Normalize a general list by normalizing each element
fn normalize_list(expr: &Value, env: &NormEnv) -> Value {
    match expr {
        Value::Pair(car, cdr) => {
            let norm_car = normalize_with_env(car, env);
            let norm_cdr = normalize_list(cdr, env);
            Value::Pair(Box::new(norm_car), Box::new(norm_cdr))
        }
        Value::Nil => Value::Nil,
        _ => normalize_with_env(expr, env),
    }
}

/// Create (dv n) - a de Bruijn variable
fn make_dv(index: usize) -> Value {
    Value::Pair(
        Box::new(Value::Symbol(Symbol::intern("dv"))),
        Box::new(Value::Pair(
            Box::new(Value::Number(index as i64)),
            Box::new(Value::Nil),
        )),
    )
}

/// Create normalized fn: (fn body)
fn make_fn_normalized(body: Value) -> Value {
    Value::Pair(
        Box::new(Value::Symbol(Symbol::intern("fn"))),
        Box::new(Value::Pair(Box::new(body), Box::new(Value::Nil))),
    )
}

/// Create normalized let: (let (val) body)
fn make_let_normalized(val: Value, body: Value) -> Value {
    Value::Pair(
        Box::new(Value::Symbol(Symbol::intern("let"))),
        Box::new(Value::Pair(
            Box::new(Value::Pair(Box::new(val), Box::new(Value::Nil))),
            Box::new(Value::Pair(Box::new(body), Box::new(Value::Nil))),
        )),
    )
}

/// Create normalized fix: (fix body)
fn make_fix_normalized(body: Value) -> Value {
    Value::Pair(
        Box::new(Value::Symbol(Symbol::intern("fix"))),
        Box::new(Value::Pair(Box::new(body), Box::new(Value::Nil))),
    )
}

/// Collect free variables in an expression (before normalization).
pub fn free_vars(expr: &Value) -> Vec<Symbol> {
    let mut result = free_vars_with_env(expr, &NormEnv::new());
    result.sort();
    result.dedup();
    result
}

fn free_vars_with_env(expr: &Value, env: &NormEnv) -> Vec<Symbol> {
    match expr {
        Value::Symbol(sym) => {
            if env.lookup(sym).is_some() {
                vec![]
            } else {
                vec![*sym]
            }
        }

        Value::Pair(car, cdr) => {
            if let Value::Symbol(head) = car.as_ref() {
                match head.as_str().as_str() {
                    "quote" => vec![],
                    "fn" => {
                        if let Some((var, body)) = parse_fn(cdr) {
                            free_vars_with_env(&body, &env.extend(var))
                        } else {
                            free_vars_list(expr, env)
                        }
                    }
                    "let" => {
                        if let Some((var, val, body)) = parse_let(cdr) {
                            let mut vars = free_vars_with_env(&val, env);
                            vars.extend(free_vars_with_env(&body, &env.extend(var)));
                            vars
                        } else {
                            free_vars_list(expr, env)
                        }
                    }
                    "fix" => {
                        if let Some((f, body)) = parse_fix(cdr) {
                            free_vars_with_env(&body, &env.extend(f))
                        } else {
                            free_vars_list(expr, env)
                        }
                    }
                    // prim is a special form: (prim op args...)
                    // op is a primitive name (not a variable), skip it
                    "prim" => {
                        if let Value::Pair(_, args_cdr) = cdr.as_ref() {
                            // Skip the op, collect free vars from remaining args
                            free_vars_list(args_cdr, env)
                        } else {
                            vec![]
                        }
                    }
                    // if, case, block, etc. - just collect from all subexpressions
                    "if" | "case" | "block" => free_vars_list(cdr, env),
                    _ => free_vars_list(expr, env),
                }
            } else {
                free_vars_list(expr, env)
            }
        }

        _ => vec![],
    }
}

fn free_vars_list(expr: &Value, env: &NormEnv) -> Vec<Symbol> {
    match expr {
        Value::Pair(car, cdr) => {
            let mut vars = free_vars_with_env(car, env);
            vars.extend(free_vars_list(cdr, env));
            vars
        }
        Value::Nil => vec![],
        _ => free_vars_with_env(expr, env),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(s: &str) -> Value {
        Value::Symbol(Symbol::intern(s))
    }

    fn list(items: &[Value]) -> Value {
        let mut result = Value::Nil;
        for item in items.iter().rev() {
            result = Value::Pair(Box::new(item.clone()), Box::new(result));
        }
        result
    }

    fn make_fn(var: &str, body: Value) -> Value {
        list(&[sym("fn"), list(&[sym(var)]), body])
    }

    #[test]
    fn test_identity_fn() {
        // (fn (x) x) -> (fn (dv 0))
        let expr = make_fn("x", sym("x"));
        let normalized = normalize(&expr);

        // Check structure: (fn (dv 0))
        let expected = list(&[sym("fn"), list(&[sym("dv"), Value::Number(0)])]);
        assert_eq!(format!("{:?}", normalized), format!("{:?}", expected));
    }

    #[test]
    fn test_nested_fn_outer_ref() {
        // (fn (x) (fn (y) x)) -> (fn (fn (dv 1)))
        let expr = make_fn("x", make_fn("y", sym("x")));
        let normalized = normalize(&expr);

        let inner = list(&[sym("fn"), list(&[sym("dv"), Value::Number(1)])]);
        let expected = list(&[sym("fn"), inner]);
        assert_eq!(format!("{:?}", normalized), format!("{:?}", expected));
    }

    #[test]
    fn test_nested_fn_inner_ref() {
        // (fn (x) (fn (y) y)) -> (fn (fn (dv 0)))
        let expr = make_fn("x", make_fn("y", sym("y")));
        let normalized = normalize(&expr);

        let inner = list(&[sym("fn"), list(&[sym("dv"), Value::Number(0)])]);
        let expected = list(&[sym("fn"), inner]);
        assert_eq!(format!("{:?}", normalized), format!("{:?}", expected));
    }

    #[test]
    fn test_free_var() {
        // (fn (x) z) -> (fn z) (z is free)
        let expr = make_fn("x", sym("z"));
        let normalized = normalize(&expr);

        let expected = list(&[sym("fn"), sym("z")]);
        assert_eq!(format!("{:?}", normalized), format!("{:?}", expected));
    }

    #[test]
    fn test_alpha_equivalence() {
        // (fn (x) x) and (fn (y) y) should normalize to the same thing
        let expr1 = make_fn("x", sym("x"));
        let expr2 = make_fn("y", sym("y"));

        let norm1 = normalize(&expr1);
        let norm2 = normalize(&expr2);

        assert_eq!(format!("{:?}", norm1), format!("{:?}", norm2));
    }

    #[test]
    fn test_free_vars() {
        // (fn (x) (+ x y)) has free var y (and +)
        let body = list(&[sym("+"), sym("x"), sym("y")]);
        let expr = make_fn("x", body);

        let fv = free_vars(&expr);
        assert!(fv.contains(&Symbol::intern("+")));
        assert!(fv.contains(&Symbol::intern("y")));
        assert!(!fv.contains(&Symbol::intern("x")));
    }
}
