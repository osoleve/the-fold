//! De Bruijn expansion with symbol supply
//!
//! The inverse of normalize: given a de Bruijn form and a supply
//! of symbols, produce an S-expression with named variables.
//!
//! ```text
//! (expand '(fn (dv 0)) '(x))        -> (fn (x) x)
//! (expand '(fn (fn (dv 1))) '(x y)) -> (fn (x) (fn (y) x))
//! ```
//!
//! Capture avoidance: expansion must not choose binder names that
//! capture free variables in the body. The symbol supply should
//! avoid names that appear free in the expression.

use crate::fabric::{symbol::Symbol, value::Value};

/// A symbol supply provides fresh names for binders.
#[derive(Clone)]
pub struct SymbolSupply {
    symbols: Vec<Symbol>,
    index: usize,
}

impl SymbolSupply {
    /// Create a symbol supply from a list of symbols.
    pub fn new(symbols: Vec<Symbol>) -> Self {
        Self { symbols, index: 0 }
    }

    /// Take the next symbol from the supply.
    pub fn next_symbol(&mut self) -> Option<Symbol> {
        if self.index < self.symbols.len() {
            let sym = self.symbols[self.index].clone();
            self.index += 1;
            Some(sym)
        } else {
            None
        }
    }
}

/// Generate a list of symbols: x, y, z, a, b, c, f, g, h, x1, y1, ...
pub fn make_symbol_supply(n: usize) -> Vec<Symbol> {
    let bases = ["x", "y", "z", "a", "b", "c", "f", "g", "h"];
    let mut result = Vec::with_capacity(n);
    for i in 0..n {
        let cycle = i / bases.len();
        let base = bases[i % bases.len()];
        let name = if cycle == 0 {
            base.to_string()
        } else {
            format!("{}{}", base, cycle)
        };
        result.push(name);
    }
    result
}

/// Expand a de Bruijn form back to named form.
pub fn expand(expr: &Value, symbols: &[Symbol]) -> Value {
    let mut supply = SymbolSupply::new(symbols.to_vec());
    expand_with_ctx(expr, &[], &mut supply)
}

/// Context maps de Bruijn index to symbol (innermost first).
fn expand_with_ctx(expr: &Value, ctx: &[Symbol], supply: &mut SymbolSupply) -> Value {
    match expr {
        // (dv n) -> the symbol at index n in context
        Value::Pair(car, cdr) => {
            if let Value::Symbol(head) = car.as_ref() {
                match head.as_str() {
                    "dv" => {
                        if let Some(idx) = parse_dv(cdr)
                            && idx < ctx.len()
                        {
                            return Value::Symbol(ctx[idx].clone());
                        }
                        // Invalid dv, return as-is
                        expr.clone()
                    }

                    // (fn body) -> (fn (var) expanded-body)
                    "fn" => {
                        if let Some(body) = parse_normalized_fn(cdr) {
                            if let Some(var) = supply.next_symbol() {
                                let mut new_ctx = vec![var.clone()];
                                new_ctx.extend(ctx.iter().cloned());
                                let expanded_body = expand_with_ctx(&body, &new_ctx, supply);
                                make_fn_named(&var, expanded_body)
                            } else {
                                // Out of symbols
                                expr.clone()
                            }
                        } else {
                            expand_list(expr, ctx, supply)
                        }
                    }

                    // (let (val) body) -> (let ((var val)) expanded-body)
                    "let" => {
                        if let Some((val, body)) = parse_normalized_let(cdr) {
                            if let Some(var) = supply.next_symbol() {
                                let expanded_val = expand_with_ctx(&val, ctx, supply);
                                let mut new_ctx = vec![var.clone()];
                                new_ctx.extend(ctx.iter().cloned());
                                let expanded_body = expand_with_ctx(&body, &new_ctx, supply);
                                make_let_named(&var, expanded_val, expanded_body)
                            } else {
                                expr.clone()
                            }
                        } else {
                            expand_list(expr, ctx, supply)
                        }
                    }

                    // (fix body) -> (fix (f) expanded-body)
                    "fix" => {
                        if let Some(body) = parse_normalized_fix(cdr) {
                            if let Some(f) = supply.next_symbol() {
                                let mut new_ctx = vec![f.clone()];
                                new_ctx.extend(ctx.iter().cloned());
                                let expanded_body = expand_with_ctx(&body, &new_ctx, supply);
                                make_fix_named(&f, expanded_body)
                            } else {
                                expr.clone()
                            }
                        } else {
                            expand_list(expr, ctx, supply)
                        }
                    }

                    // (quote datum) -> unchanged
                    "quote" => expr.clone(),

                    // Other forms: expand each element
                    _ => expand_list(expr, ctx, supply),
                }
            } else {
                expand_list(expr, ctx, supply)
            }
        }

        // Symbols (free variables) pass through
        Value::Symbol(_) => expr.clone(),

        // Non-list atoms pass through
        _ => expr.clone(),
    }
}

/// Parse (dv n) - returns the index
fn parse_dv(cdr: &Value) -> Option<usize> {
    let (idx_val, rest) = uncons(cdr)?;
    if !is_nil(&rest) {
        return None;
    }
    match idx_val {
        Value::Number(n) if n >= 0 => Some(n as usize),
        _ => None,
    }
}

/// Parse normalized fn: (fn body) - cdr is (body)
fn parse_normalized_fn(cdr: &Value) -> Option<Value> {
    let (body, rest) = uncons(cdr)?;
    if !is_nil(&rest) {
        return None;
    }
    Some(body)
}

/// Parse normalized let: (let (val) body) - cdr is ((val) body)
fn parse_normalized_let(cdr: &Value) -> Option<(Value, Value)> {
    let (val_list, rest) = uncons(cdr)?;
    let (body, rest2) = uncons(&rest)?;
    if !is_nil(&rest2) {
        return None;
    }
    let (val, val_rest) = uncons(&val_list)?;
    if !is_nil(&val_rest) {
        return None;
    }
    Some((val, body))
}

/// Parse normalized fix: (fix body) - cdr is (body)
fn parse_normalized_fix(cdr: &Value) -> Option<Value> {
    parse_normalized_fn(cdr)
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

/// Expand a general list by expanding each element
fn expand_list(expr: &Value, ctx: &[Symbol], supply: &mut SymbolSupply) -> Value {
    match expr {
        Value::Pair(car, cdr) => {
            let exp_car = expand_with_ctx(car, ctx, supply);
            let exp_cdr = expand_list(cdr, ctx, supply);
            Value::Pair(Box::new(exp_car), Box::new(exp_cdr))
        }
        Value::Nil => Value::Nil,
        _ => expand_with_ctx(expr, ctx, supply),
    }
}

/// Create (fn (var) body) with named variable
fn make_fn_named(var: &Symbol, body: Value) -> Value {
    list(&[
        Value::Symbol("fn".to_string()),
        list(&[Value::Symbol(var.clone())]),
        body,
    ])
}

/// Create (let ((var val)) body) with named variable
fn make_let_named(var: &Symbol, val: Value, body: Value) -> Value {
    list(&[
        Value::Symbol("let".to_string()),
        list(&[list(&[Value::Symbol(var.clone()), val])]),
        body,
    ])
}

/// Create (fix (f) body) with named variable
fn make_fix_named(f: &Symbol, body: Value) -> Value {
    list(&[
        Value::Symbol("fix".to_string()),
        list(&[Value::Symbol(f.clone())]),
        body,
    ])
}

/// Helper to build a proper list from values
fn list(items: &[Value]) -> Value {
    let mut result = Value::Nil;
    for item in items.iter().rev() {
        result = Value::Pair(Box::new(item.clone()), Box::new(result));
    }
    result
}

/// Expand using a fresh symbol supply (convenience function).
pub fn expand_fresh(expr: &Value) -> Value {
    let symbols = make_symbol_supply(100);
    expand(expr, &symbols)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fabric::normalize::normalize;

    fn sym(s: &str) -> Value {
        Value::Symbol(s.to_string())
    }

    fn make_fn(var: &str, body: Value) -> Value {
        list(&[sym("fn"), list(&[sym(var)]), body])
    }

    #[test]
    fn test_expand_identity() {
        // normalize: (fn (x) x) -> (fn (dv 0))
        // expand with (a): (fn (dv 0)) -> (fn (a) a)
        let orig = make_fn("x", sym("x"));
        let normalized = normalize(&orig);
        let expanded = expand(&normalized, &["a".to_string()]);

        // Should be (fn (a) a)
        let expected = make_fn("a", sym("a"));
        assert_eq!(format!("{:?}", expanded), format!("{:?}", expected));
    }

    #[test]
    fn test_roundtrip() {
        // (fn (x) (fn (y) x)) should roundtrip
        let orig = make_fn("x", make_fn("y", sym("x")));
        let normalized = normalize(&orig);
        let expanded = expand(&normalized, &["a".to_string(), "b".to_string()]);

        // Should be (fn (a) (fn (b) a))
        let expected = make_fn("a", make_fn("b", sym("a")));
        assert_eq!(format!("{:?}", expanded), format!("{:?}", expected));
    }

    #[test]
    fn test_expand_fresh() {
        let orig = make_fn("foo", sym("foo"));
        let normalized = normalize(&orig);
        let expanded = expand_fresh(&normalized);

        // Default supply starts with x
        let expected = make_fn("x", sym("x"));
        assert_eq!(format!("{:?}", expanded), format!("{:?}", expected));
    }
}
