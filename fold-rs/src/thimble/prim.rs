use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    rc::Rc,
    sync::{Mutex, OnceLock},
};

use num_bigint::BigInt;
use num_rational::BigRational;

use crate::fabric::{
    address::{ADDRESS_SIZE, Address},
    bigint, bigrational,
    block::Block,
    cas::{Store, address_to_hex, hash_block, hex_to_address},
    error::EvalError,
    sha256::sha256,
    symbol::Symbol,
    value::Value,
};

static CAS_STORE: OnceLock<Mutex<Store>> = OnceLock::new();

fn cas_store() -> &'static Mutex<Store> {
    CAS_STORE.get_or_init(|| Mutex::new(Store::new()))
}

// Session management for daemon
static CURRENT_SESSION_ID: OnceLock<Mutex<Option<String>>> = OnceLock::new();

fn session_mutex() -> &'static Mutex<Option<String>> {
    CURRENT_SESSION_ID.get_or_init(|| Mutex::new(None))
}

/// Set the current session ID (called from daemon before evaluation)
pub fn set_current_session_id(session_id: Option<String>) {
    if let Ok(mut guard) = session_mutex().lock() {
        *guard = session_id;
    }
}

/// Get the current session ID
fn get_current_session_id() -> Option<String> {
    session_mutex().lock().ok().and_then(|guard| guard.clone())
}

/// Session file path for given session ID
fn session_file_path(session_id: &str) -> String {
    format!(".fold-sessions/{}.session", session_id)
}

/// Read session data from file (returns alist as Value)
fn read_session_file(session_id: &str) -> Option<Value> {
    let path = session_file_path(session_id);
    std::fs::read_to_string(&path).ok().and_then(|content| {
        // Parse simple S-expression alist
        parse_session_alist(&content)
    })
}

/// Write session data to file
fn write_session_file(
    session_id: &str,
    tier: &str,
    model: &str,
    name: &str,
) -> std::io::Result<()> {
    // Ensure directory exists
    std::fs::create_dir_all(".fold-sessions")?;
    let path = session_file_path(session_id);
    let content = format!(
        "((tier . {}) (model . {}) (name . {}) (session-id . \"{}\"))",
        tier, model, name, session_id
    );
    std::fs::write(&path, content)
}

/// Delete session file
fn delete_session_file(session_id: &str) -> std::io::Result<()> {
    let path = session_file_path(session_id);
    if std::path::Path::new(&path).exists() {
        std::fs::remove_file(&path)
    } else {
        Ok(())
    }
}

/// Parse a simple session alist like ((tier . symbol) (name . symbol) ...)
fn parse_session_alist(s: &str) -> Option<Value> {
    let s = s.trim();
    if !s.starts_with("((") || !s.ends_with("))") {
        return None;
    }
    // Extract key-value pairs
    let inner = &s[1..s.len() - 1]; // Remove outer parens
    let mut pairs = Vec::new();
    let mut depth = 0;
    let mut current = String::new();

    for c in inner.chars() {
        match c {
            '(' => {
                depth += 1;
                current.push(c);
            }
            ')' => {
                depth -= 1;
                current.push(c);
                if depth == 0 && !current.trim().is_empty() {
                    if let Some(pair) = parse_pair(current.trim()) {
                        pairs.push(pair);
                    }
                    current.clear();
                }
            }
            _ => current.push(c),
        }
    }

    // Build alist as proper list of pairs
    let mut result = Value::Nil;
    for pair in pairs.into_iter().rev() {
        result = Value::Pair(Box::new(pair), Box::new(result));
    }
    Some(result)
}

/// Parse a single pair like (tier . symbol)
fn parse_pair(s: &str) -> Option<Value> {
    let s = s.trim();
    if !s.starts_with('(') || !s.ends_with(')') {
        return None;
    }
    let inner = s[1..s.len() - 1].trim();
    let parts: Vec<&str> = inner.splitn(3, " . ").collect();
    if parts.len() != 2 {
        return None;
    }
    let key = parts[0].trim();
    let value_str = parts[1].trim();

    let key_value = Value::Symbol(Symbol::from(key));
    let value = if value_str.starts_with('"') && value_str.ends_with('"') {
        Value::String(value_str[1..value_str.len() - 1].to_string())
    } else {
        Value::Symbol(Symbol::from(value_str))
    };

    Some(Value::Pair(Box::new(key_value), Box::new(value)))
}

/// Map model tier to role
fn model_to_role(model: &str) -> Option<&'static str> {
    match model {
        "opus" => Some("shepherd"),
        "sonnet" => Some("builder"),
        "haiku" => Some("player"),
        // Also accept role names directly
        "shepherd" => Some("shepherd"),
        "builder" => Some("builder"),
        "player" => Some("player"),
        _ => None,
    }
}

pub fn apply_prim(op: &Symbol, args: &[Value]) -> Result<Value, EvalError> {
    match op.as_str().as_str() {
        // Arithmetic (Scheme operator aliases)
        "+" => fold_numeric(args, 0, 0.0, |a, b| a + b, |a, b| a + b),
        "-" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("- expects at least 1 arg"));
            }
            let first = expect_number(&args[0])?;
            if args.len() == 1 {
                return Ok(numeric_to_value(negate_numeric(first)));
            }
            reduce_numeric(first, &args[1..], |a, b| a - b, |a, b| a - b)
        }
        "*" => fold_numeric(args, 1, 1.0, |a, b| a * b, |a, b| a * b),
        "/" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("/ expects 2 args"));
            }
            let a = expect_number(&args[0])?;
            let b = expect_number(&args[1])?;
            match (a, b) {
                (Numeric::Int(left), Numeric::Int(right)) => {
                    if right == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    Ok(Value::Number(left / right))
                }
                _ => {
                    let af = a.as_f64();
                    let bf = b.as_f64();
                    if bf == 0.0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    Ok(Value::Float(af / bf))
                }
            }
        }
        // Arithmetic (alt names)
        "add" => fold_numeric(args, 0, 0.0, |a, b| a + b, |a, b| a + b),
        "sub" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("sub expects at least 1 arg"));
            }
            let first = expect_number(&args[0])?;
            if args.len() == 1 {
                return Ok(numeric_to_value(negate_numeric(first)));
            }
            reduce_numeric(first, &args[1..], |a, b| a - b, |a, b| a - b)
        }
        "mul" => fold_numeric(args, 1, 1.0, |a, b| a * b, |a, b| a * b),
        "div" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("div expects 2 args"));
            }
            let a = expect_number(&args[0])?;
            let b = expect_number(&args[1])?;
            match (a, b) {
                (Numeric::Int(left), Numeric::Int(right)) => {
                    if right == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    Ok(Value::Number(left / right))
                }
                _ => {
                    let right = b.as_f64();
                    if right == 0.0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    Ok(Value::Float(a.as_f64() / right))
                }
            }
        }
        "mod" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("mod expects 2 args"));
            }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Number(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("quotient expects 2 args"));
            }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Number(a / b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("remainder expects 2 args"));
            }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Number(a % b))
        }
        "neg" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("neg expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            Ok(numeric_to_value(negate_numeric(n)))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("abs expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            Ok(numeric_to_value(abs_numeric(n)))
        }
        "sqrt" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sqrt expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.sqrt()))
        }
        "expt" | "pow" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("expt expects 2 args"));
            }
            let base = expect_number(&args[0])?;
            let exp = expect_number(&args[1])?;
            match (base, exp) {
                (Numeric::Int(b), Numeric::Int(e)) if e >= 0 => Ok(Value::Number(b.pow(e as u32))),
                _ => Ok(Value::Float(base.as_f64().powf(exp.as_f64()))),
            }
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("min expects at least 1 arg"));
            }
            let mut min_val = expect_number(&args[0])?;
            for arg in &args[1..] {
                let val = expect_number(arg)?;
                if val.as_f64() < min_val.as_f64() {
                    min_val = val;
                }
            }
            Ok(numeric_to_value(min_val))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("max expects at least 1 arg"));
            }
            let mut max_val = expect_number(&args[0])?;
            for arg in &args[1..] {
                let val = expect_number(arg)?;
                if val.as_f64() > max_val.as_f64() {
                    max_val = val;
                }
            }
            Ok(numeric_to_value(max_val))
        }
        "gcd" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("gcd expects 2 args"));
            }
            let a = expect_integer(&args[0])?.abs();
            let b = expect_integer(&args[1])?.abs();
            fn gcd_impl(mut a: i64, mut b: i64) -> i64 {
                while b != 0 {
                    let temp = b;
                    b = a % b;
                    a = temp;
                }
                a
            }
            Ok(Value::Number(gcd_impl(a, b)))
        }
        "lcm" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("lcm expects 2 args"));
            }
            let a = expect_integer(&args[0])?.abs();
            let b = expect_integer(&args[1])?.abs();
            fn gcd_impl(mut a: i64, mut b: i64) -> i64 {
                while b != 0 {
                    let temp = b;
                    b = a % b;
                    a = temp;
                }
                a
            }
            if a == 0 || b == 0 {
                Ok(Value::Number(0))
            } else {
                Ok(Value::Number((a / gcd_impl(a, b)) * b))
            }
        }
        "clamp" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "clamp expects 3 args: (clamp val min max)",
                ));
            }
            let val = expect_number(&args[0])?;
            let min_val = expect_number(&args[1])?;
            let max_val = expect_number(&args[2])?;
            let val_f = val.as_f64();
            let min_f = min_val.as_f64();
            let max_f = max_val.as_f64();
            let clamped = val_f.max(min_f).min(max_f);
            if clamped.fract() == 0.0 && clamped.is_finite() {
                Ok(Value::Number(clamped as i64))
            } else {
                Ok(Value::Float(clamped))
            }
        }
        "between?" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "between? expects 3 args: (between? val min max)",
                ));
            }
            let val = expect_number(&args[0])?;
            let min_val = expect_number(&args[1])?;
            let max_val = expect_number(&args[2])?;
            let result = val.as_f64() >= min_val.as_f64() && val.as_f64() <= max_val.as_f64();
            Ok(Value::Bool(result))
        }
        "sign" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sign expects 1 arg"));
            }
            let val = expect_number(&args[0])?.as_f64();
            let sign = if val > 0.0 {
                1
            } else if val < 0.0 {
                -1
            } else {
                0
            };
            Ok(Value::Number(sign))
        }
        "divmod" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "divmod expects 2 args: (divmod a b)",
                ));
            }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            let quotient = a / b;
            let remainder = a % b;
            Ok(list_from_values(&[
                Value::Number(quotient),
                Value::Number(remainder),
            ]))
        }
        // Interpolation and scaling utilities
        "lerp" => {
            // Linear interpolation: (lerp a b t) = a + (b - a) * t
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch("lerp expects 3 args: (lerp a b t)"));
            }
            let a = expect_number(&args[0])?.as_f64();
            let b = expect_number(&args[1])?.as_f64();
            let t = expect_number(&args[2])?.as_f64();
            Ok(Value::Float(a + (b - a) * t))
        }
        "inverse-lerp" => {
            // Inverse linear interpolation: find t given a, b, and value
            // (inverse-lerp a b v) = (v - a) / (b - a)
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "inverse-lerp expects 3 args: (inverse-lerp a b value)",
                ));
            }
            let a = expect_number(&args[0])?.as_f64();
            let b = expect_number(&args[1])?.as_f64();
            let v = expect_number(&args[2])?.as_f64();
            if (b - a).abs() < 1e-10 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Float((v - a) / (b - a)))
        }
        "scale" => {
            // Scale value from one range to another
            // (scale v in-min in-max out-min out-max)
            if args.len() != 5 {
                return Err(EvalError::TypeMismatch(
                    "scale expects 5 args: (scale v in-min in-max out-min out-max)",
                ));
            }
            let v = expect_number(&args[0])?.as_f64();
            let in_min = expect_number(&args[1])?.as_f64();
            let in_max = expect_number(&args[2])?.as_f64();
            let out_min = expect_number(&args[3])?.as_f64();
            let out_max = expect_number(&args[4])?.as_f64();
            if (in_max - in_min).abs() < 1e-10 {
                return Err(EvalError::DivisionByZero);
            }
            let t = (v - in_min) / (in_max - in_min);
            Ok(Value::Float(out_min + (out_max - out_min) * t))
        }
        "smoothstep" => {
            // Smooth Hermite interpolation: (smoothstep edge0 edge1 x)
            // Returns 0 if x < edge0, 1 if x > edge1, smooth curve otherwise
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "smoothstep expects 3 args: (smoothstep edge0 edge1 x)",
                ));
            }
            let edge0 = expect_number(&args[0])?.as_f64();
            let edge1 = expect_number(&args[1])?.as_f64();
            let x = expect_number(&args[2])?.as_f64();
            if (edge1 - edge0).abs() < 1e-10 {
                return Ok(Value::Float(if x < edge0 { 0.0 } else { 1.0 }));
            }
            let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
            Ok(Value::Float(t * t * (3.0 - 2.0 * t)))
        }
        "denormalize" => {
            // Denormalize: expand from [0,1] to [min,max]
            // (denormalize t min max) = min + t * (max - min)
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "denormalize expects 3 args: (denormalize t min max)",
                ));
            }
            let t = expect_number(&args[0])?.as_f64();
            let min_val = expect_number(&args[1])?.as_f64();
            let max_val = expect_number(&args[2])?.as_f64();
            Ok(Value::Float(min_val + t * (max_val - min_val)))
        }
        "saturation-add" => {
            // Add with saturation/clamping to range
            // (saturation-add a b min max)
            if args.len() != 4 {
                return Err(EvalError::TypeMismatch(
                    "saturation-add expects 4 args: (saturation-add a b min max)",
                ));
            }
            let a = expect_number(&args[0])?.as_f64();
            let b = expect_number(&args[1])?.as_f64();
            let min_val = expect_number(&args[2])?.as_f64();
            let max_val = expect_number(&args[3])?.as_f64();
            let result = (a + b).clamp(min_val, max_val);
            Ok(Value::Float(result))
        }
        "percent-of" => {
            // Calculate percentage: (percent-of part whole) = part / whole * 100
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "percent-of expects 2 args: (percent-of part whole)",
                ));
            }
            let part = expect_number(&args[0])?.as_f64();
            let whole = expect_number(&args[1])?.as_f64();
            if whole.abs() < 1e-10 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Float(part / whole * 100.0))
        }
        "percent-change" => {
            // Calculate percentage change: (percent-change old new) = (new - old) / old * 100
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "percent-change expects 2 args: (percent-change old new)",
                ));
            }
            let old_val = expect_number(&args[0])?.as_f64();
            let new_val = expect_number(&args[1])?.as_f64();
            if old_val.abs() < 1e-10 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Float((new_val - old_val) / old_val * 100.0))
        }
        "round-to" => {
            // Round to specified decimal places: (round-to x places)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "round-to expects 2 args: (round-to x places)",
                ));
            }
            let x = expect_number(&args[0])?.as_f64();
            let places = expect_integer(&args[1])?;
            let factor = 10_f64.powi(places as i32);
            Ok(Value::Float((x * factor).round() / factor))
        }
        "log" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("log expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.ln()))
        }
        "exp" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("exp expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.exp()))
        }
        "sin" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sin expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.sin()))
        }
        "cos" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("cos expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.cos()))
        }
        "tan" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("tan expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.tan()))
        }
        "asin" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("asin expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.asin()))
        }
        "acos" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("acos expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.acos()))
        }
        "atan" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("atan expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.atan()))
        }
        "floor" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("floor expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i)),
                Numeric::Float(f) => Ok(Value::Number(f.floor() as i64)),
            }
        }
        "ceiling" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("ceiling expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i)),
                Numeric::Float(f) => Ok(Value::Number(f.ceil() as i64)),
            }
        }
        "round" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("round expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i)),
                Numeric::Float(f) => Ok(Value::Number(f.round() as i64)),
            }
        }

        // Comparison (Scheme operator aliases)
        "=" => cmp_numbers(args, |a, b| (a - b).abs() < 1e-10),
        "<" => cmp_numbers(args, |a, b| a < b),
        ">" => cmp_numbers(args, |a, b| a > b),
        "<=" => cmp_numbers(args, |a, b| a <= b),
        ">=" => cmp_numbers(args, |a, b| a >= b),
        // Comparison (alt names)
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("eq? expects 2 args"));
            }
            Ok(Value::Bool(value_eq(&args[0], &args[1])))
        }
        "lt?" => cmp_numbers(args, |a, b| a < b),
        "le?" => cmp_numbers(args, |a, b| a <= b),
        "gt?" => cmp_numbers(args, |a, b| a > b),
        "ge?" => cmp_numbers(args, |a, b| a >= b),
        "zero?" => unary_number_pred(args, |n| n == 0.0),
        "positive?" => unary_number_pred(args, |n| n > 0.0),
        "negative?" => unary_number_pred(args, |n| n < 0.0),
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("even? expects 1 arg"));
            }
            let n = expect_integer(&args[0])?;
            Ok(Value::Bool(n % 2 == 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("odd? expects 1 arg"));
            }
            let n = expect_integer(&args[0])?;
            Ok(Value::Bool(n % 2 != 0))
        }

        // Bitwise
        "bitand" => binary_number(args, |a, b| a & b),
        "bitor" => binary_number(args, |a, b| a | b),
        "bitxor" => binary_number(args, |a, b| a ^ b),
        "bitnot" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bitnot expects 1 arg"));
            }
            Ok(Value::Number(!expect_integer(&args[0])?))
        }
        "shl" => shift_number(args, |a, b| a << b),
        "shr" => shift_number(args, |a, b| a >> b),

        // Boolean
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("not expects 1 arg"));
            }
            Ok(Value::Bool(!is_truthy(&args[0])))
        }
        "and" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("and expects 2 args"));
            }
            Ok(Value::Bool(is_truthy(&args[0]) && is_truthy(&args[1])))
        }
        "or" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("or expects 2 args"));
            }
            Ok(Value::Bool(is_truthy(&args[0]) || is_truthy(&args[1])))
        }

        // List operations
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("cons expects 2 args"));
            }
            Ok(Value::Pair(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
            ))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("car expects 1 arg"));
            }
            match &args[0] {
                Value::Pair(left, _) => Ok((**left).clone()),
                _ => Err(EvalError::TypeMismatch("car expects pair")),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("cdr expects 1 arg"));
            }
            match &args[0] {
                Value::Pair(_, right) => Ok((**right).clone()),
                _ => Err(EvalError::TypeMismatch("cdr expects pair")),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("null? expects 1 arg"));
            }
            Ok(Value::Bool(matches!(args[0], Value::Nil)))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pair? expects 1 arg"));
            }
            Ok(Value::Bool(matches!(args[0], Value::Pair(_, _))))
        }
        "empty?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("empty? expects 1 arg"));
            }
            Ok(Value::Bool(matches!(args[0], Value::Nil)))
        }
        "all-equal?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("all-equal? expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Bool(true));
            }
            let first = &list[0];
            let all_same = list.iter().all(|item| value_eq(item, first));
            Ok(Value::Bool(all_same))
        }
        "list" => Ok(list_from_values(args)),
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list? expects 1 arg"));
            }
            Ok(Value::Bool(is_list(&args[0])))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("length expects 1 arg"));
            }
            let list = list_to_vec(&args[0])?;
            Ok(Value::Number(list.len() as i64))
        }
        "append" => {
            let mut items = Vec::new();
            for list in args {
                items.extend(list_to_vec(list)?);
            }
            Ok(list_from_values(&items))
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("reverse expects 1 arg"));
            }
            let mut items = list_to_vec(&args[0])?;
            items.reverse();
            Ok(list_from_values(&items))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("list-ref expects 2 args"));
            }
            let list = list_to_vec(&args[0])?;
            let idx = expect_usize(&args[1])?;
            if idx >= list.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            Ok(list[idx].clone())
        }
        "member" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("member expects 2 args"));
            }
            let needle = &args[0];
            let mut current = &args[1];
            loop {
                match current {
                    Value::Nil => return Ok(Value::Bool(false)),
                    Value::Pair(head, tail) => {
                        if value_eq(head, needle) {
                            return Ok(Value::Bool(true));
                        }
                        current = tail;
                    }
                    _ => return Err(EvalError::TypeMismatch("member expects list")),
                }
            }
        }
        "memq" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("memq expects 2 args"));
            }
            let needle = &args[0];
            let mut current = &args[1];
            loop {
                match current {
                    Value::Nil => return Ok(Value::Bool(false)),
                    Value::Pair(head, tail) => {
                        if value_eq(head, needle) {
                            return Ok(current.clone());
                        }
                        current = tail;
                    }
                    _ => return Err(EvalError::TypeMismatch("memq expects list")),
                }
            }
        }
        "assq" | "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("assq/assoc expects 2 args"));
            }
            let key = &args[0];
            let mut current = &args[1];
            loop {
                match current {
                    Value::Nil => return Ok(Value::Bool(false)),
                    Value::Pair(head, tail) => match &**head {
                        Value::Pair(car, cdr) => {
                            if value_eq(car, key) {
                                return Ok(Value::Pair(car.clone(), cdr.clone()));
                            }
                            current = tail;
                        }
                        _ => return Err(EvalError::TypeMismatch("assq/assoc expects alist")),
                    },
                    _ => return Err(EvalError::TypeMismatch("assq/assoc expects alist")),
                }
            }
        }

        // Vector operations
        "vec-make" => Ok(Value::Vector(args.to_vec())),
        "vec-empty" => Ok(Value::Vector(Vec::new())),
        "vec-ref" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("vec-ref expects 2 args"));
            }
            let values = expect_vector(&args[0])?;
            let idx = expect_usize(&args[1])?;
            if idx >= values.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            Ok(values[idx].clone())
        }
        "vec-length" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("vec-length expects 1 arg"));
            }
            let values = expect_vector(&args[0])?;
            Ok(Value::Number(values.len() as i64))
        }
        "vec->list" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("vec->list expects 1 arg"));
            }
            let values = expect_vector(&args[0])?;
            Ok(list_from_values(values))
        }
        "list->vec" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list->vec expects 1 arg"));
            }
            let list = list_to_vec(&args[0])?;
            Ok(Value::Vector(list))
        }

        // Type predicates
        "number?" => type_predicate(args, |v| {
            matches!(
                v,
                Value::Number(_) | Value::Float(_) | Value::BigInt(_) | Value::BigRational(_)
            )
        }),
        "integer?" => type_predicate(args, |v| matches!(v, Value::Number(_))),
        "bigint?" => type_predicate(args, |v| matches!(v, Value::BigInt(_))),
        "rational?" => type_predicate(args, |v| matches!(v, Value::BigRational(_))),
        "exact?" => type_predicate(args, |v| {
            matches!(
                v,
                Value::Number(_) | Value::BigInt(_) | Value::BigRational(_)
            )
        }),
        "inexact?" => type_predicate(args, |v| matches!(v, Value::Float(_))),
        "symbol?" => type_predicate(args, |v| matches!(v, Value::Symbol(_))),
        "string?" => type_predicate(args, |v| matches!(v, Value::String(_))),
        "char?" => type_predicate(args, |v| matches!(v, Value::Char(_))),
        "bytevector?" => type_predicate(args, |v| matches!(v, Value::Bytevector(_))),
        "block?" => type_predicate(args, |v| matches!(v, Value::Block(_))),
        "vector?" => type_predicate(args, |v| matches!(v, Value::Vector(_))),
        "boolean?" => type_predicate(args, |v| matches!(v, Value::Bool(_))),
        "procedure?" => type_predicate(args, |v| matches!(v, Value::Closure(_))),
        "address?" => type_predicate(args, |v| matches!(v, Value::Address(_))),

        // String operations
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-length expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Number(s.chars().count() as i64))
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("string-ref expects 2 args"));
            }
            let s = expect_string(&args[0])?;
            let idx = expect_usize(&args[1])?;
            let ch = s.chars().nth(idx).ok_or(EvalError::IndexOutOfBounds)?;
            Ok(Value::Char(ch))
        }
        "string-append" => {
            let mut out = String::new();
            for arg in args {
                out.push_str(&expect_string(arg)?);
            }
            Ok(Value::String(out))
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch("substring expects 3 args"));
            }
            let s = expect_string(&args[0])?;
            let start = expect_usize(&args[1])?;
            let end = expect_usize(&args[2])?;
            let chars: Vec<char> = s.chars().collect();
            if start > end || end > chars.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            let out: String = chars[start..end].iter().collect();
            Ok(Value::String(out))
        }
        "string=?" => binary_string_cmp(args, |a, b| a == b),
        "string<?" => binary_string_cmp(args, |a, b| a < b),
        "string>?" => binary_string_cmp(args, |a, b| a > b),
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::TypeMismatch("make-string expects 1 or 2 args"));
            }
            let len = expect_usize(&args[0])?;
            let ch = if args.len() == 2 {
                expect_char(&args[1])?
            } else {
                ' '
            };
            Ok(Value::String(std::iter::repeat_n(ch, len).collect()))
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string->list expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            let items = s.chars().map(Value::Char).collect::<Vec<_>>();
            Ok(list_from_values(&items))
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list->string expects 1 arg"));
            }
            let list = list_to_vec(&args[0])?;
            let mut out = String::new();
            for value in list {
                let ch = match value {
                    Value::Char(ch) => ch,
                    _ => return Err(EvalError::TypeMismatch("list->string expects chars")),
                };
                out.push(ch);
            }
            Ok(Value::String(out))
        }

        // Character operations
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char->integer expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Number(ch as u32 as i64))
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("integer->char expects 1 arg"));
            }
            let code = expect_integer(&args[0])?;
            let code =
                u32::try_from(code).map_err(|_| EvalError::TypeMismatch("invalid char code"))?;
            let ch = char::from_u32(code).ok_or(EvalError::TypeMismatch("invalid char code"))?;
            Ok(Value::Char(ch))
        }
        "char=?" => binary_char_cmp(args, |a, b| a == b),
        "char<?" => binary_char_cmp(args, |a, b| a < b),
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-alphabetic? expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Bool(ch.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-numeric? expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Bool(ch.is_numeric()))
        }
        "char-whitespace?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-whitespace? expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Bool(ch.is_whitespace()))
        }
        "char-upper-case?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-upper-case? expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Bool(ch.is_uppercase()))
        }
        "char-lower-case?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-lower-case? expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Bool(ch.is_lowercase()))
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-upcase expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Char(ch.to_uppercase().next().unwrap_or(ch)))
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("char-downcase expects 1 arg"));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Char(ch.to_lowercase().next().unwrap_or(ch)))
        }

        // String/Bytevector conversion
        "string->utf8" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string->utf8 expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bytevector(s.into_bytes()))
        }
        "utf8->string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("utf8->string expects 1 arg"));
            }
            let bytes = expect_bytevector(&args[0])?;
            let s =
                String::from_utf8(bytes).map_err(|_| EvalError::TypeMismatch("invalid utf8"))?;
            Ok(Value::String(s))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("symbol->string expects 1 arg"));
            }
            let sym = expect_symbol(&args[0])?;
            Ok(Value::String(sym.as_str()))
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string->symbol expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Symbol(Symbol::intern(&s)))
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("number->string expects 1 arg"));
            }
            let n = expect_number(&args[0])?;
            let s = match n {
                Numeric::Int(n) => n.to_string(),
                Numeric::Float(n) => n.to_string(),
            };
            Ok(Value::String(s))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string->number expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            let trimmed = s.trim();
            match trimmed.parse::<i64>() {
                Ok(n) => Ok(Value::Number(n)),
                Err(_) => match trimmed.parse::<f64>() {
                    Ok(n) => Ok(Value::Float(n)),
                    Err(_) => Ok(Value::Bool(false)),
                },
            }
        }

        // Bytevector operations
        "bv-length" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bv-length expects 1 arg"));
            }
            let bytes = expect_bytevector(&args[0])?;
            Ok(Value::Number(bytes.len() as i64))
        }
        "bv-ref" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bv-ref expects 2 args"));
            }
            let bytes = expect_bytevector(&args[0])?;
            let idx = expect_usize(&args[1])?;
            if idx >= bytes.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            Ok(Value::Number(bytes[idx] as i64))
        }
        "bv-make" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::TypeMismatch("bv-make expects 1 or 2 args"));
            }
            let len = expect_usize(&args[0])?;
            let fill = if args.len() == 2 {
                let value = expect_integer(&args[1])?;
                if !(0..=255).contains(&value) {
                    return Err(EvalError::TypeMismatch("bytevector fill out of range"));
                }
                value as u8
            } else {
                0u8
            };
            Ok(Value::Bytevector(vec![fill; len]))
        }
        "bv-concat" => {
            let mut out = Vec::new();
            for arg in args {
                let bytes = expect_bytevector(arg)?;
                out.extend_from_slice(&bytes);
            }
            Ok(Value::Bytevector(out))
        }
        "bv-slice" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch("bv-slice expects 3 args"));
            }
            let bytes = expect_bytevector(&args[0])?;
            let start = expect_usize(&args[1])?;
            let end = expect_usize(&args[2])?;
            if start > end || end > bytes.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            Ok(Value::Bytevector(bytes[start..end].to_vec()))
        }
        "bv-copy" => {
            // (prim 'bv-copy src src-start dst dst-start count)
            if args.len() != 5 {
                return Err(EvalError::TypeMismatch("bv-copy expects 5 args"));
            }
            let src = expect_bytevector(&args[0])?;
            let src_start = expect_usize(&args[1])?;
            let mut dst = expect_bytevector(&args[2])?;
            let dst_start = expect_usize(&args[3])?;
            let count = expect_usize(&args[4])?;
            if src_start + count > src.len() || dst_start + count > dst.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            dst[dst_start..dst_start + count].copy_from_slice(&src[src_start..src_start + count]);
            Ok(Value::Bytevector(dst))
        }

        // Hash operations
        "sha256" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sha256 expects 1 arg"));
            }
            let bytes = expect_bytevector(&args[0])?;
            Ok(Value::Bytevector(sha256(&bytes).to_vec()))
        }
        "hash-block" => {
            let block = expect_block(args)?;
            Ok(Value::Address(hash_block(block)))
        }
        "hash->hex" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hash->hex expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            Ok(Value::String(address_to_hex(&address)))
        }
        "hex->hash" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hex->hash expects 1 arg"));
            }
            let hex = expect_string(&args[0])?;
            let address = hex_to_address(&hex)
                .map_err(|_| EvalError::TypeMismatch("hex->hash expects address hex"))?;
            Ok(Value::Address(address))
        }

        // CAS store operations
        "store!" => {
            let block = expect_block(args)?;
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let address = store.store(block.clone());
            Ok(Value::Address(address))
        }
        "fetch" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("fetch expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            match store.fetch(&address) {
                Some(block) => Ok(Value::Block(block.clone())),
                None => Ok(Value::Bool(false)),
            }
        }
        "stored?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("stored? expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            Ok(Value::Bool(store.stored(&address)))
        }
        "pin!" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pin! expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            store.pin(address);
            Ok(Value::Nil)
        }
        "pinned?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pinned? expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            Ok(Value::Bool(store.pinned(&address)))
        }
        "unpin!" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("unpin! expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            store.unpin(&address);
            Ok(Value::Nil)
        }
        "pin-tree!" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pin-tree! expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let count = store.pin_tree(&address);
            Ok(Value::Number(count as i64))
        }
        "unpin-tree!" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("unpin-tree! expects 1 arg"));
            }
            let address = to_address(&args[0])?;
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let count = store.unpin_tree(&address);
            Ok(Value::Number(count as i64))
        }
        "gc!" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("gc! expects 0 args"));
            }
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let (collected, remaining) = store.gc();
            let values = [
                Value::Number(collected as i64),
                Value::Number(remaining as i64),
            ];
            Ok(list_from_values(&values))
        }
        "gc-with-roots!" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("gc-with-roots! expects 1 arg"));
            }
            let roots = list_to_vec(&args[0])?;
            let mut addresses = Vec::with_capacity(roots.len());
            for root in roots {
                addresses.push(to_address(&root)?);
            }
            let mut store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let (collected, remaining) = store.gc_with_roots(&addresses);
            let values = [
                Value::Number(collected as i64),
                Value::Number(remaining as i64),
            ];
            Ok(list_from_values(&values))
        }
        "store-count" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("store-count expects 0 args"));
            }
            let store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            Ok(Value::Number(store.store_count() as i64))
        }
        "store-hashes" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("store-hashes expects 0 args"));
            }
            let store = cas_store()
                .lock()
                .map_err(|_| EvalError::Unimplemented("cas store unavailable"))?;
            let hashes = store
                .store_hashes()
                .into_iter()
                .map(Value::Address)
                .collect::<Vec<_>>();
            Ok(list_from_values(&hashes))
        }

        // Block operations
        "make-block" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch("make-block expects 3 args"));
            }
            let tag = expect_symbol(&args[0])?;
            let payload = expect_bytevector(&args[1])?;
            let refs = expect_vector(&args[2])?
                .iter()
                .map(to_address)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::Block(Block::new(tag, payload, refs)))
        }
        // Simple block constructor: (block tag val1 val2 ...)
        // Creates a block with empty payload and values as refs (embedded as hashes)
        "block" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("block expects at least tag"));
            }
            let tag = expect_symbol(&args[0])?;
            // Convert remaining args to addresses by hashing their representation
            let refs = args[1..]
                .iter()
                .map(|v| {
                    // Use a simple hash based on the value's string representation
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    format!("{:?}", v).hash(&mut hasher);
                    let hash = hasher.finish();
                    let mut addr = [0u8; 33];
                    addr[0] = 0x00; // version byte
                    addr[1..9].copy_from_slice(&hash.to_le_bytes());
                    Address(addr)
                })
                .collect::<Vec<_>>();
            Ok(Value::Block(Block::new(tag, vec![], refs)))
        }
        "block-tag" => {
            let block = expect_block(args)?;
            Ok(Value::Symbol(block.tag.clone()))
        }
        "block-payload" => {
            let block = expect_block(args)?;
            Ok(Value::Bytevector(block.payload.clone()))
        }
        "block-refs" => {
            let block = expect_block(args)?;
            let refs = block.refs.iter().copied().map(Value::Address).collect();
            Ok(Value::Vector(refs))
        }
        "block->bytes" => {
            let block = expect_block(args)?;
            Ok(Value::Bytevector(block.to_bytes()))
        }
        "bytes->block" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bytes->block expects 1 arg"));
            }
            let bytes = expect_bytevector(&args[0])?;
            let block = Block::from_bytes(&bytes)
                .map_err(|_| EvalError::TypeMismatch("bytes->block expects block bytes"))?;
            Ok(Value::Block(block))
        }
        "block-ref" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("block-ref expects 2 args"));
            }
            let block = match &args[0] {
                Value::Block(block) => block,
                _ => return Err(EvalError::TypeMismatch("block-ref expects block")),
            };
            let idx = expect_usize(&args[1])?;
            if idx >= block.refs.len() {
                return Err(EvalError::IndexOutOfBounds);
            }
            Ok(Value::Address(block.refs[idx]))
        }

        // Normalization operations
        "normalize" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("normalize expects 1 arg"));
            }
            Ok(crate::fabric::normalize::normalize(&args[0]))
        }
        "expand" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("expand expects 2 args"));
            }
            let symbols = list_to_vec(&args[1])?
                .into_iter()
                .map(|v| match v {
                    Value::Symbol(s) => Ok(s),
                    _ => Err(EvalError::TypeMismatch("expand expects symbol list")),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(crate::fabric::expand::expand(&args[0], &symbols))
        }
        "free-vars" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("free-vars expects 1 arg"));
            }
            let vars = crate::fabric::normalize::free_vars(&args[0]);
            let values: Vec<Value> = vars.into_iter().map(Value::Symbol).collect();
            Ok(list_from_values(&values))
        }

        // ============================================================
        // BigInt operations
        // ============================================================

        // Construction
        "bigint" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint expects 1 arg"));
            }
            match &args[0] {
                Value::Number(n) => Ok(Value::BigInt(bigint::bigint_from_i64(*n))),
                Value::BigInt(n) => Ok(Value::BigInt(n.clone())),
                Value::String(s) => bigint::bigint_from_string(s)
                    .map(Value::BigInt)
                    .map_err(|_| EvalError::TypeMismatch("invalid bigint string")),
                _ => Err(EvalError::TypeMismatch("bigint expects number or string")),
            }
        }
        "bigint->string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint->string expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::String(bigint::bigint_to_string(n)))
        }
        "bigint->number" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint->number expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            match bigint::bigint_to_i64(n) {
                Some(i) => Ok(Value::Number(i)),
                None => Err(EvalError::TypeMismatch("bigint too large for i64")),
            }
        }

        // Arithmetic
        "bigint-add" => bigint_binary(args, bigint::bigint_add),
        "bigint-sub" => bigint_binary(args, bigint::bigint_sub),
        "bigint-mul" => bigint_binary(args, bigint::bigint_mul),
        "bigint-neg" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-neg expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::BigInt(bigint::bigint_neg(n)))
        }
        "bigint-abs" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-abs expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::BigInt(bigint::bigint_abs(n)))
        }

        // Division
        "bigint-quotient" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-quotient expects 2 args"));
            }
            let a = expect_bigint(&args[0])?;
            let b = expect_bigint(&args[1])?;
            bigint::bigint_quotient(a, b)
                .map(Value::BigInt)
                .map_err(|_| EvalError::DivisionByZero)
        }
        "bigint-remainder" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-remainder expects 2 args"));
            }
            let a = expect_bigint(&args[0])?;
            let b = expect_bigint(&args[1])?;
            bigint::bigint_remainder(a, b)
                .map(Value::BigInt)
                .map_err(|_| EvalError::DivisionByZero)
        }
        "bigint-divmod" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-divmod expects 2 args"));
            }
            let a = expect_bigint(&args[0])?;
            let b = expect_bigint(&args[1])?;
            let (q, r) = bigint::bigint_divmod(a, b).map_err(|_| EvalError::DivisionByZero)?;
            Ok(Value::Pair(
                Box::new(Value::BigInt(q)),
                Box::new(Value::BigInt(r)),
            ))
        }

        // Comparison
        "bigint=?" => bigint_cmp(args, bigint::bigint_eq),
        "bigint<?" => bigint_cmp(args, bigint::bigint_lt),
        "bigint<=?" => bigint_cmp(args, bigint::bigint_le),
        "bigint>?" => bigint_cmp(args, bigint::bigint_gt),
        "bigint>=?" => bigint_cmp(args, bigint::bigint_ge),
        "bigint-zero?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-zero? expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::Bool(bigint::bigint_is_zero(n)))
        }
        "bigint-positive?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-positive? expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::Bool(bigint::bigint_is_positive(n)))
        }
        "bigint-negative?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-negative? expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::Bool(bigint::bigint_is_negative(n)))
        }

        // GCD/LCM
        "bigint-gcd" => bigint_binary(args, bigint::bigint_gcd),
        "bigint-lcm" => bigint_binary(args, bigint::bigint_lcm),
        "bigint-extended-gcd" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "bigint-extended-gcd expects 2 args",
                ));
            }
            let a = expect_bigint(&args[0])?;
            let b = expect_bigint(&args[1])?;
            let (gcd, x, y) = bigint::bigint_extended_gcd(a, b);
            let values = [Value::BigInt(gcd), Value::BigInt(x), Value::BigInt(y)];
            Ok(list_from_values(&values))
        }

        // Bitwise
        "bigint-and" => bigint_binary(args, bigint::bigint_and),
        "bigint-or" => bigint_binary(args, bigint::bigint_or),
        "bigint-xor" => bigint_binary(args, bigint::bigint_xor),
        "bigint-shl" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-shl expects 2 args"));
            }
            let n = expect_bigint(&args[0])?;
            let shift = expect_u32(&args[1])?;
            Ok(Value::BigInt(bigint::bigint_shl(n, shift)))
        }
        "bigint-shr" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-shr expects 2 args"));
            }
            let n = expect_bigint(&args[0])?;
            let shift = expect_u32(&args[1])?;
            Ok(Value::BigInt(bigint::bigint_shr(n, shift)))
        }
        "bigint-bits" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("bigint-bits expects 1 arg"));
            }
            let n = expect_bigint(&args[0])?;
            Ok(Value::Number(bigint::bigint_bits(n) as i64))
        }

        // Power
        "bigint-pow" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("bigint-pow expects 2 args"));
            }
            let base = expect_bigint(&args[0])?;
            let exp = expect_u32(&args[1])?;
            Ok(Value::BigInt(bigint::bigint_pow(base, exp)))
        }
        "bigint-modpow" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch("bigint-modpow expects 3 args"));
            }
            let base = expect_bigint(&args[0])?;
            let exp = expect_bigint(&args[1])?;
            let modulus = expect_bigint(&args[2])?;
            bigint::bigint_modpow(base, exp, modulus)
                .map(Value::BigInt)
                .map_err(|_| EvalError::DivisionByZero)
        }

        // ============================================================
        // BigRational operations
        // ============================================================

        // Construction
        "make-rational" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("make-rational expects 2 args"));
            }
            let numer = to_bigint(&args[0])?;
            let denom = to_bigint(&args[1])?;
            bigrational::bigrational_new(numer, denom)
                .map(Value::BigRational)
                .map_err(|_| EvalError::DivisionByZero)
        }
        "rational" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational expects 1 arg"));
            }
            match &args[0] {
                Value::BigRational(r) => Ok(Value::BigRational(r.clone())),
                Value::BigInt(n) => Ok(Value::BigRational(bigrational::bigrational_from_bigint(
                    n.clone(),
                ))),
                Value::Number(n) => Ok(Value::BigRational(bigrational::bigrational_from_i64(*n))),
                _ => Err(EvalError::TypeMismatch(
                    "rational expects number or rational",
                )),
            }
        }

        // Accessors
        "rational-numerator" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-numerator expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_numer(r)))
        }
        "rational-denominator" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "rational-denominator expects 1 arg",
                ));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_denom(r)))
        }

        // Arithmetic
        "rational-add" => rational_binary(args, bigrational::bigrational_add),
        "rational-sub" => rational_binary(args, bigrational::bigrational_sub),
        "rational-mul" => rational_binary(args, bigrational::bigrational_mul),
        "rational-div" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("rational-div expects 2 args"));
            }
            let a = expect_rational(&args[0])?;
            let b = expect_rational(&args[1])?;
            bigrational::bigrational_div(a, b)
                .map(Value::BigRational)
                .map_err(|_| EvalError::DivisionByZero)
        }
        "rational-neg" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-neg expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigRational(bigrational::bigrational_neg(r)))
        }
        "rational-abs" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-abs expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigRational(bigrational::bigrational_abs(r)))
        }
        "rational-recip" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-recip expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            bigrational::bigrational_recip(r)
                .map(Value::BigRational)
                .map_err(|_| EvalError::DivisionByZero)
        }

        // Comparison
        "rational=?" => rational_cmp(args, bigrational::bigrational_eq),
        "rational<?" => rational_cmp(args, bigrational::bigrational_lt),
        "rational<=?" => rational_cmp(args, bigrational::bigrational_le),
        "rational>?" => rational_cmp(args, bigrational::bigrational_gt),
        "rational>=?" => rational_cmp(args, bigrational::bigrational_ge),
        "rational-zero?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-zero? expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::Bool(bigrational::bigrational_is_zero(r)))
        }
        "rational-positive?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-positive? expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::Bool(bigrational::bigrational_is_positive(r)))
        }
        "rational-negative?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-negative? expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::Bool(bigrational::bigrational_is_negative(r)))
        }
        "rational-integer?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-integer? expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::Bool(bigrational::bigrational_is_integer(r)))
        }

        // Rounding
        "rational-floor" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-floor expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_floor(r)))
        }
        "rational-ceiling" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-ceiling expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_ceil(r)))
        }
        "rational-truncate" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-truncate expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_trunc(r)))
        }
        "rational-round" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational-round expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::BigInt(bigrational::bigrational_round(r)))
        }

        // Conversion
        "rational->float" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rational->float expects 1 arg"));
            }
            let r = expect_rational(&args[0])?;
            Ok(Value::Float(bigrational::bigrational_to_f64(r)))
        }

        // Hashtable operations
        "make-hashtable" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("make-hashtable expects 0 args"));
            }
            Ok(Value::Hashtable(Rc::new(RefCell::new(HashMap::new()))))
        }
        "hashtable?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hashtable? expects 1 arg"));
            }
            Ok(Value::Bool(matches!(args[0], Value::Hashtable(_))))
        }
        "hashtable-set!" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "hashtable-set! expects 3 args: (hashtable-set! ht key value)",
                ));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let key = args[1].clone();
                    let value = args[2].clone();
                    ht.borrow_mut().insert(key, value);
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::TypeMismatch("hashtable-set! expects hashtable")),
            }
        }
        "hashtable-ref" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "hashtable-ref expects 3 args: (hashtable-ref ht key default)",
                ));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let key = &args[1];
                    let default = &args[2];
                    Ok(ht.borrow().get(key).cloned().unwrap_or_else(|| default.clone()))
                }
                _ => Err(EvalError::TypeMismatch("hashtable-ref expects hashtable")),
            }
        }
        "hashtable-contains?" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "hashtable-contains? expects 2 args: (hashtable-contains? ht key)",
                ));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let key = &args[1];
                    Ok(Value::Bool(ht.borrow().contains_key(key)))
                }
                _ => Err(EvalError::TypeMismatch("hashtable-contains? expects hashtable")),
            }
        }
        "hashtable-delete!" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "hashtable-delete! expects 2 args: (hashtable-delete! ht key)",
                ));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let key = &args[1];
                    ht.borrow_mut().remove(key);
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::TypeMismatch("hashtable-delete! expects hashtable")),
            }
        }
        "hashtable-keys" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hashtable-keys expects 1 arg"));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let keys: Vec<Value> = ht.borrow().keys().cloned().collect();
                    Ok(list_from_values(&keys))
                }
                _ => Err(EvalError::TypeMismatch("hashtable-keys expects hashtable")),
            }
        }
        "hashtable-values" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hashtable-values expects 1 arg"));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let values: Vec<Value> = ht.borrow().values().cloned().collect();
                    Ok(list_from_values(&values))
                }
                _ => Err(EvalError::TypeMismatch("hashtable-values expects hashtable")),
            }
        }
        "hashtable-size" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hashtable-size expects 1 arg"));
            }
            match &args[0] {
                Value::Hashtable(ht) => Ok(Value::Number(ht.borrow().len() as i64)),
                _ => Err(EvalError::TypeMismatch("hashtable-size expects hashtable")),
            }
        }
        "hashtable->alist" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hashtable->alist expects 1 arg"));
            }
            match &args[0] {
                Value::Hashtable(ht) => {
                    let pairs: Vec<Value> = ht
                        .borrow()
                        .iter()
                        .map(|(k, v)| Value::Pair(Box::new(k.clone()), Box::new(v.clone())))
                        .collect();
                    Ok(list_from_values(&pairs))
                }
                _ => Err(EvalError::TypeMismatch("hashtable->alist expects hashtable")),
            }
        }
        "alist->hashtable" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("alist->hashtable expects 1 arg"));
            }
            let mut ht = HashMap::new();
            let mut current = &args[0];
            loop {
                match current {
                    Value::Nil => break,
                    Value::Pair(car, cdr) => {
                        // Each element should be a pair (key . value)
                        match car.as_ref() {
                            Value::Pair(key, value) => {
                                ht.insert(key.as_ref().clone(), value.as_ref().clone());
                            }
                            _ => {
                                return Err(EvalError::TypeMismatch(
                                    "alist->hashtable expects list of pairs",
                                ))
                            }
                        }
                        current = cdr;
                    }
                    _ => {
                        return Err(EvalError::TypeMismatch(
                            "alist->hashtable expects proper list",
                        ))
                    }
                }
            }
            Ok(Value::Hashtable(Rc::new(RefCell::new(ht))))
        }

        // REPL commands
        "version" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("version expects 0 args"));
            }
            Ok(Value::String(
                "The Fold GENESIS\nContent-Addressed Storage and Merkle Log Forum System"
                    .to_string(),
            ))
        }
        "help" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("help expects 0 args"));
            }
            Ok(Value::String(
                "The Fold - Available Commands:\n\
                  (version)  - Show system version\n\
                  (who)      - Show session info\n\
                  (help)     - Show this help\n\
                \n\
                For full functionality, use the Scheme daemon (FOLD_USE_SCHEME=1).\n\
                The Rust daemon supports the core Fold language primitives."
                    .to_string(),
            ))
        }
        // IO primitives
        "display" => {
            // Print value without quotes (human-readable)
            for arg in args {
                print!("{}", value_to_display_string(arg));
            }
            Ok(Value::Nil)
        }
        "write" => {
            // Print value with quotes (machine-readable)
            for arg in args {
                print!("{}", value_to_write_string(arg));
            }
            Ok(Value::Nil)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("newline expects 0 args"));
            }
            println!();
            Ok(Value::Nil)
        }

        // String utilities
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-upcase expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-downcase expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.to_lowercase()))
        }
        "upcase" | "upcase-string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("upcase expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.to_uppercase()))
        }
        "downcase" | "downcase-string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("downcase expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.to_lowercase()))
        }
        "string-starts-with?" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-starts-with? expects 2 args",
                ));
            }
            let s = expect_string(&args[0])?;
            let prefix = expect_string(&args[1])?;
            Ok(Value::Bool(s.starts_with(&prefix)))
        }
        "string-ends-with?" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("string-ends-with? expects 2 args"));
            }
            let s = expect_string(&args[0])?;
            let suffix = expect_string(&args[1])?;
            Ok(Value::Bool(s.ends_with(&suffix)))
        }
        "string-contains?" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("string-contains? expects 2 args"));
            }
            let s = expect_string(&args[0])?;
            let substring = expect_string(&args[1])?;
            Ok(Value::Bool(s.contains(&substring)))
        }
        "string-split" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("string-split expects 2 args"));
            }
            let s = expect_string(&args[0])?;
            let delim = expect_string(&args[1])?;
            let parts: Vec<Value> = s
                .split(&delim)
                .map(|part| Value::String(part.to_string()))
                .collect();
            Ok(list_from_values(&parts))
        }
        "string-trim" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-trim expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim().to_string()))
        }
        "string-ltrim" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-ltrim expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim_start().to_string()))
        }
        "string-rtrim" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-rtrim expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim_end().to_string()))
        }
        "string-pad" => {
            if args.len() != 2 && args.len() != 3 {
                return Err(EvalError::TypeMismatch("string-pad expects 2 or 3 args"));
            }
            let s = expect_string(&args[0])?;
            let width = expect_usize(&args[1])?;
            let pad_char = if args.len() == 3 {
                expect_char(&args[2])?
            } else {
                ' '
            };

            if s.len() >= width {
                Ok(Value::String(s))
            } else {
                let padding = pad_char.to_string().repeat(width - s.len());
                Ok(Value::String(format!("{}{}", padding, s)))
            }
        }
        "string-reverse" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string-reverse expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.chars().rev().collect()))
        }
        "string-repeat" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-repeat expects 2 args: (string-repeat str count)",
                ));
            }
            let s = expect_string(&args[0])?;
            let count = expect_usize(&args[1])?;
            Ok(Value::String(s.repeat(count)))
        }
        "string-replace" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "string-replace expects 3 args: (string-replace str old new)",
                ));
            }
            let s = expect_string(&args[0])?;
            let old = expect_string(&args[1])?;
            let new = expect_string(&args[2])?;
            Ok(Value::String(s.replace(&old, &new)))
        }
        "string-index" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-index expects 2 args: (string-index str substring)",
                ));
            }
            let s = expect_string(&args[0])?;
            let substring = expect_string(&args[1])?;
            match s.find(&substring) {
                Some(idx) => Ok(Value::Number(idx as i64)),
                None => Ok(Value::Nil),
            }
        }
        "string-rindex" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-rindex expects 2 args: (string-rindex str substring)",
                ));
            }
            let s = expect_string(&args[0])?;
            let substring = expect_string(&args[1])?;
            match s.rfind(&substring) {
                Some(idx) => Ok(Value::Number(idx as i64)),
                None => Ok(Value::Nil),
            }
        }
        "pad-left" => {
            if args.len() != 2 && args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "pad-left expects 2 or 3 args: (pad-left str width [pad-char])",
                ));
            }
            let s = expect_string(&args[0])?;
            let width = expect_usize(&args[1])?;
            let pad_char = if args.len() == 3 {
                expect_char(&args[2])?
            } else {
                ' '
            };
            if s.len() >= width {
                Ok(Value::String(s))
            } else {
                let padding = pad_char.to_string().repeat(width - s.len());
                Ok(Value::String(format!("{}{}", padding, s)))
            }
        }
        "pad-right" => {
            if args.len() != 2 && args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "pad-right expects 2 or 3 args: (pad-right str width [pad-char])",
                ));
            }
            let s = expect_string(&args[0])?;
            let width = expect_usize(&args[1])?;
            let pad_char = if args.len() == 3 {
                expect_char(&args[2])?
            } else {
                ' '
            };
            if s.len() >= width {
                Ok(Value::String(s))
            } else {
                let padding = pad_char.to_string().repeat(width - s.len());
                Ok(Value::String(format!("{}{}", s, padding)))
            }
        }
        "string-center" => {
            if args.len() != 2 && args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "string-center expects 2 or 3 args: (string-center str width [pad-char])",
                ));
            }
            let s = expect_string(&args[0])?;
            let width = expect_usize(&args[1])?;
            let pad_char = if args.len() == 3 {
                expect_char(&args[2])?
            } else {
                ' '
            };
            if s.len() >= width {
                Ok(Value::String(s))
            } else {
                let total_pad = width - s.len();
                let left_pad = total_pad / 2;
                let right_pad = total_pad - left_pad;
                let left = pad_char.to_string().repeat(left_pad);
                let right = pad_char.to_string().repeat(right_pad);
                Ok(Value::String(format!("{}{}{}", left, s, right)))
            }
        }
        "char-code" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "char-code expects 1 arg: a character",
                ));
            }
            let ch = expect_char(&args[0])?;
            Ok(Value::Number(ch as i64))
        }
        "code-char" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "code-char expects 1 arg: an integer",
                ));
            }
            let code = expect_integer(&args[0])?;
            if !((0..=1_114_111).contains(&code)) {
                return Err(EvalError::TypeMismatch(
                    "code-char: value out of valid Unicode range",
                ));
            }
            if let Some(ch) = char::from_u32(code as u32) {
                Ok(Value::Char(ch))
            } else {
                Err(EvalError::TypeMismatch(
                    "code-char: invalid Unicode codepoint",
                ))
            }
        }
        "chars" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("chars expects 1 arg: a string"));
            }
            let s = expect_string(&args[0])?;
            let char_values: Vec<Value> = s.chars().map(Value::Char).collect();
            Ok(list_from_values(&char_values))
        }
        "string-concat" => {
            // Concatenate all arguments as strings
            let mut result = String::new();
            for arg in args {
                result.push_str(&value_to_display_string(arg));
            }
            Ok(Value::String(result))
        }
        "list-copy" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list-copy expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            Ok(list_from_values(&list))
        }
        "make-list" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::TypeMismatch(
                    "make-list expects 1 or 2 args: (make-list n [value])",
                ));
            }
            let n = expect_usize(&args[0])?;
            let fill_value = if args.len() > 1 {
                args[1].clone()
            } else {
                Value::Nil
            };
            let list = vec![fill_value; n];
            Ok(list_from_values(&list))
        }

        // Format string
        "format" => {
            if args.is_empty() {
                return Err(EvalError::TypeMismatch("format expects at least 1 arg"));
            }
            let format_str = expect_string(&args[0])?;
            let mut result = String::new();
            let mut arg_index = 1;
            let mut chars = format_str.chars().peekable();

            while let Some(ch) = chars.next() {
                if ch == '~' {
                    if let Some(&next) = chars.peek() {
                        match next {
                            'a' | 's' => {
                                // ~a and ~s both convert to string (atom/string format)
                                chars.next();
                                if arg_index < args.len() {
                                    result.push_str(&value_to_display_string(&args[arg_index]));
                                    arg_index += 1;
                                } else {
                                    result.push('~');
                                    result.push(next);
                                }
                            }
                            'd' => {
                                // ~d for decimal integer
                                chars.next();
                                if arg_index < args.len() {
                                    if let Ok(n) = expect_number(&args[arg_index]) {
                                        match n {
                                            Numeric::Int(i) => result.push_str(&i.to_string()),
                                            Numeric::Float(f) => {
                                                result.push_str(&(f as i64).to_string())
                                            }
                                        }
                                    }
                                    arg_index += 1;
                                } else {
                                    result.push('~');
                                    result.push(next);
                                }
                            }
                            'n' => {
                                // ~n for newline
                                chars.next();
                                result.push('\n');
                            }
                            '~' => {
                                // ~~ for literal tilde
                                chars.next();
                                result.push('~');
                            }
                            _ => {
                                result.push(ch);
                            }
                        }
                    } else {
                        result.push(ch);
                    }
                } else {
                    result.push(ch);
                }
            }

            Ok(Value::String(result))
        }

        // List utilities
        "foldr" => {
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "foldr expects 3 args: (foldr f init lst)",
                ));
            }
            // foldr requires calling a closure, which happens at eval time
            // This would need special handling in the evaluator
            Err(EvalError::TypeMismatch(
                "foldr requires closure evaluation (not yet supported in Rust core)",
            ))
        }
        "take" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("take expects 2 args"));
            }
            let n = expect_usize(&args[1])?;
            let mut items = Vec::new();
            let mut current = &args[0];
            for _ in 0..n {
                match current {
                    Value::Nil => break,
                    Value::Pair(head, tail) => {
                        items.push(*head.clone());
                        current = tail;
                    }
                    _ => return Err(EvalError::TypeMismatch("take expects a list")),
                }
            }
            Ok(list_from_values(&items))
        }
        "drop" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("drop expects 2 args"));
            }
            let n = expect_usize(&args[1])?;
            let mut current = &args[0];
            for _ in 0..n {
                match current {
                    Value::Nil => return Ok(Value::Nil),
                    Value::Pair(_, tail) => {
                        current = tail;
                    }
                    _ => return Err(EvalError::TypeMismatch("drop expects a list")),
                }
            }
            Ok(current.clone())
        }
        "last" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("last expects 1 arg"));
            }
            let mut current = &args[0];
            let mut last_val = None;
            loop {
                match current {
                    Value::Nil => {
                        return match last_val {
                            Some(val) => Ok(val),
                            None => Err(EvalError::TypeMismatch("last expects a non-empty list")),
                        };
                    }
                    Value::Pair(head, tail) => {
                        last_val = Some(*head.clone());
                        current = tail;
                    }
                    _ => return Err(EvalError::TypeMismatch("last expects a list")),
                }
            }
        }
        "flatten" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("flatten expects 1 arg"));
            }
            fn flatten_list(val: &Value) -> Vec<Value> {
                match val {
                    Value::Nil => vec![],
                    Value::Pair(head, tail) => {
                        let mut result = flatten_list(head);
                        result.extend(flatten_list(tail));
                        result
                    }
                    other => vec![other.clone()],
                }
            }
            let flattened = flatten_list(&args[0]);
            Ok(list_from_values(&flattened))
        }
        "range" => {
            // (range end) - generates 0 to end-1
            // (range start end) - generates start to end-1
            // (range start end step) - generates start to end-1 with step
            if args.is_empty() || args.len() > 3 {
                return Err(EvalError::TypeMismatch(
                    "range expects 1-3 args: (range end) or (range start end) or (range start end step)",
                ));
            }

            let (start, end, step) = if args.len() == 1 {
                (0i64, expect_integer(&args[0])?, 1i64)
            } else if args.len() == 2 {
                (expect_integer(&args[0])?, expect_integer(&args[1])?, 1i64)
            } else {
                (
                    expect_integer(&args[0])?,
                    expect_integer(&args[1])?,
                    expect_integer(&args[2])?,
                )
            };

            if step == 0 {
                return Err(EvalError::TypeMismatch("range: step cannot be zero"));
            }

            let mut result = Vec::new();
            if step > 0 {
                let mut i = start;
                while i < end {
                    result.push(Value::Number(i));
                    i += step;
                }
            } else {
                let mut i = start;
                while i > end {
                    result.push(Value::Number(i));
                    i += step;
                }
            }

            Ok(list_from_values(&result))
        }
        "repeat" => {
            // (repeat n elem) - creates a list with elem repeated n times
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "repeat expects 2 args: (repeat count elem)",
                ));
            }
            let count = expect_integer(&args[0])? as usize;
            let elem = &args[1];
            let result: Vec<Value> = (0..count).map(|_| elem.clone()).collect();
            Ok(list_from_values(&result))
        }
        "sum" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sum expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            let mut total: f64 = 0.0;
            for item in list {
                let n = expect_number(&item)?;
                total += n.as_f64();
            }
            // Return as integer if it's a whole number, otherwise float
            if total.fract() == 0.0 && total.is_finite() {
                Ok(Value::Number(total as i64))
            } else {
                Ok(Value::Float(total))
            }
        }
        "product" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("product expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            let mut total: f64 = 1.0;
            for item in list {
                let n = expect_number(&item)?;
                total *= n.as_f64();
            }
            // Return as integer if it's a whole number, otherwise float
            if total.fract() == 0.0 && total.is_finite() {
                Ok(Value::Number(total as i64))
            } else {
                Ok(Value::Float(total))
            }
        }
        "sum-of" | "max-of" | "min-of" => {
            // These are aliases that do the same thing as sum/product but with -of suffix
            // Actually, let's implement max-of and min-of properly
            if op == "max-of" {
                if args.len() != 1 {
                    return Err(EvalError::TypeMismatch(
                        "max-of expects 1 arg: a non-empty list",
                    ));
                }
                let list = list_to_vec(&args[0])?;
                if list.is_empty() {
                    return Err(EvalError::TypeMismatch(
                        "max-of: cannot find max of empty list",
                    ));
                }
                let mut max_val = expect_number(&list[0])?;
                for item in &list[1..] {
                    let val = expect_number(item)?;
                    if val.as_f64() > max_val.as_f64() {
                        max_val = val;
                    }
                }
                Ok(numeric_to_value(max_val))
            } else if op == "min-of" {
                if args.len() != 1 {
                    return Err(EvalError::TypeMismatch(
                        "min-of expects 1 arg: a non-empty list",
                    ));
                }
                let list = list_to_vec(&args[0])?;
                if list.is_empty() {
                    return Err(EvalError::TypeMismatch(
                        "min-of: cannot find min of empty list",
                    ));
                }
                let mut min_val = expect_number(&list[0])?;
                for item in &list[1..] {
                    let val = expect_number(item)?;
                    if val.as_f64() < min_val.as_f64() {
                        min_val = val;
                    }
                }
                Ok(numeric_to_value(min_val))
            } else {
                // sum-of is just an alias for sum
                if args.len() != 1 {
                    return Err(EvalError::TypeMismatch("sum-of expects 1 arg: a list"));
                }
                let list = list_to_vec(&args[0])?;
                let mut total: f64 = 0.0;
                for item in list {
                    let n = expect_number(&item)?;
                    total += n.as_f64();
                }
                if total.fract() == 0.0 && total.is_finite() {
                    Ok(Value::Number(total as i64))
                } else {
                    Ok(Value::Float(total))
                }
            }
        }
        "sort" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "sort expects 1 arg: a list of numbers",
                ));
            }
            let mut list = list_to_vec(&args[0])?;
            // Sort numbers numerically
            list.sort_by(|a, b| {
                let a_num = match expect_number(a) {
                    Ok(n) => n.as_f64(),
                    Err(_) => f64::NAN,
                };
                let b_num = match expect_number(b) {
                    Ok(n) => n.as_f64(),
                    Err(_) => f64::NAN,
                };
                a_num
                    .partial_cmp(&b_num)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            Ok(list_from_values(&list))
        }
        "unique" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("unique expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            let mut seen = Vec::new();
            for item in list {
                if !seen.iter().any(|v| value_eq(v, &item)) {
                    seen.push(item);
                }
            }
            Ok(list_from_values(&seen))
        }
        "find-index" | "index-of" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "find-index expects 2 args: (find-index lst elem)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let needle = &args[1];
            for (i, item) in list.iter().enumerate() {
                if value_eq(item, needle) {
                    return Ok(Value::Number(i as i64));
                }
            }
            Ok(Value::Nil)
        }
        "count" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "count expects 2 args: (count lst elem)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let needle = &args[1];
            let count = list.iter().filter(|item| value_eq(item, needle)).count();
            Ok(Value::Number(count as i64))
        }
        "nth" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("nth expects 2 args: (nth n lst)"));
            }
            let n = expect_usize(&args[0])?;
            let list = list_to_vec(&args[1])?;
            if n < list.len() {
                Ok(list[n].clone())
            } else {
                Ok(Value::Nil)
            }
        }
        "zip" => {
            if args.len() < 2 {
                return Err(EvalError::TypeMismatch("zip expects at least 2 lists"));
            }
            // Convert all arguments to vectors
            let mut lists: Vec<Vec<Value>> = Vec::new();
            for arg in args {
                lists.push(list_to_vec(arg)?);
            }
            // Find minimum length
            let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
            // Create zipped list of lists
            let mut result = Vec::new();
            for i in 0..min_len {
                let mut tuple = Vec::new();
                for list in &lists {
                    tuple.push(list[i].clone());
                }
                result.push(list_from_values(&tuple));
            }
            Ok(list_from_values(&result))
        }
        "unzip" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "unzip expects 1 arg: a list of lists",
                ));
            }
            let list_of_lists = list_to_vec(&args[0])?;
            if list_of_lists.is_empty() {
                return Ok(Value::Nil);
            }
            // Get the first sublist to determine size
            let first = list_to_vec(&list_of_lists[0])?;
            let num_cols = first.len();
            // Create columns
            let mut columns: Vec<Vec<Value>> = vec![Vec::new(); num_cols];
            for item in list_of_lists {
                let sublist = list_to_vec(&item)?;
                if sublist.len() != num_cols {
                    return Err(EvalError::TypeMismatch(
                        "unzip: all sublists must have same length",
                    ));
                }
                for (col_idx, val) in sublist.iter().enumerate() {
                    columns[col_idx].push(val.clone());
                }
            }
            // Create result list of lists
            let result: Vec<Value> = columns
                .into_iter()
                .map(|col| list_from_values(&col))
                .collect();
            Ok(list_from_values(&result))
        }
        "rotate" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "rotate expects 2 args: (rotate lst n)",
                ));
            }
            let mut list = list_to_vec(&args[0])?;
            let n = expect_integer(&args[1])?;
            if list.is_empty() {
                return Ok(list_from_values(&list));
            }
            let len = list.len() as i64;
            let normalized = ((n % len) + len) % len; // Handle negative rotations
            list.rotate_left(normalized as usize);
            Ok(list_from_values(&list))
        }
        "list->set" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list->set expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            let mut seen = Vec::new();
            for item in list {
                if !seen.iter().any(|v| value_eq(v, &item)) {
                    seen.push(item);
                }
            }
            Ok(list_from_values(&seen))
        }
        "interleave" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("interleave expects 2 lists"));
            }
            let list1 = list_to_vec(&args[0])?;
            let list2 = list_to_vec(&args[1])?;
            let mut result = Vec::new();
            let max_len = list1.len().max(list2.len());
            for i in 0..max_len {
                if i < list1.len() {
                    result.push(list1[i].clone());
                }
                if i < list2.len() {
                    result.push(list2[i].clone());
                }
            }
            Ok(list_from_values(&result))
        }
        "list->pairs" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list->pairs expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            let mut result = Vec::new();
            for i in (0..list.len()).step_by(2) {
                if i + 1 < list.len() {
                    let pair = list_from_values(&[list[i].clone(), list[i + 1].clone()]);
                    result.push(pair);
                } else {
                    result.push(list[i].clone());
                }
            }
            Ok(list_from_values(&result))
        }
        "init" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("init expects 1 arg: a list"));
            }
            let mut list = list_to_vec(&args[0])?;
            if !list.is_empty() {
                list.pop();
            }
            Ok(list_from_values(&list))
        }
        "int->float" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "int->float expects 1 arg: an integer",
                ));
            }
            let n = expect_integer(&args[0])?;
            Ok(Value::Float(n as f64))
        }
        "float->int" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("float->int expects 1 arg: a float"));
            }
            let f = expect_number(&args[0])?.as_f64();
            Ok(Value::Number(f as i64))
        }
        "identity" => {
            // Return first argument unchanged (useful as a default/identity function)
            if args.is_empty() {
                Ok(Value::Nil)
            } else {
                Ok(args[0].clone())
            }
        }
        "apply" => {
            // Apply a function to a list of arguments
            // (apply f '(a b c)) => (f a b c)
            // (apply f x y '(a b c)) => (f x y a b c)
            if args.len() < 2 {
                return Err(EvalError::TypeMismatch("apply expects at least 2 args"));
            }
            let func = &args[0];
            let last_arg = &args[args.len() - 1];

            // Collect middle arguments (if any)
            let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();

            // Append the list arguments
            let list_args = list_to_vec(last_arg)?;
            call_args.extend(list_args);

            // Apply based on function type
            match func {
                Value::Primitive(op) => apply_prim(op, &call_args),
                Value::Closure(_) => {
                    // Closures need to be applied by the evaluator
                    // Return a special error that could be caught
                    Err(EvalError::TypeMismatch(
                        "apply: use direct call syntax for closures",
                    ))
                }
                _ => Err(EvalError::TypeMismatch(
                    "apply: first arg must be a function",
                )),
            }
        }
        "xor" => {
            // Logical XOR: true if exactly one argument is truthy
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("xor expects 2 args"));
            }
            let a_truthy = !matches!(args[0], Value::Bool(false));
            let b_truthy = !matches!(args[1], Value::Bool(false));
            Ok(Value::Bool(a_truthy ^ b_truthy))
        }
        "gensym" => {
            // Generate a unique symbol (simple version using a counter)
            // This is a simplified version - real implementation would track global state
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("gensym expects 0 args"));
            }
            use std::sync::atomic::{AtomicU64, Ordering};
            static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
            let id = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
            Ok(Value::Symbol(Symbol::intern(&format!("g{}", id))))
        }
        "error" => {
            // Throw an error (ignores message due to EvalError type constraints)
            Err(EvalError::TypeMismatch("error raised"))
        }
        "deep-equal?" => {
            // Deep structural equality (like equal? in Scheme)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("deep-equal? expects 2 args"));
            }
            Ok(Value::Bool(value_eq(&args[0], &args[1])))
        }
        "list-every?" => {
            // Check if all elements equal a value (requires manual predicate)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "list-every? expects 2 args: (list-every? lst value)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let check_value = &args[1];
            let all_match = list.iter().all(|item| value_eq(item, check_value));
            Ok(Value::Bool(all_match))
        }
        "first" => {
            // Get first element of list (alias for car)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("first expects 1 arg: a list"));
            }
            match &args[0] {
                Value::Pair(head, _) => Ok((**head).clone()),
                Value::Nil => Ok(Value::Nil),
                _ => Err(EvalError::TypeMismatch("first expects a list")),
            }
        }
        "rest" => {
            // Get rest of list (alias for cdr)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("rest expects 1 arg: a list"));
            }
            match &args[0] {
                Value::Pair(_, tail) => Ok((**tail).clone()),
                Value::Nil => Ok(Value::Nil),
                _ => Err(EvalError::TypeMismatch("rest expects a list")),
            }
        }
        "second" => {
            // Get second element (nth 1)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("second expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            if list.len() > 1 {
                Ok(list[1].clone())
            } else {
                Ok(Value::Nil)
            }
        }
        "third" => {
            // Get third element (nth 2)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("third expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            if list.len() > 2 {
                Ok(list[2].clone())
            } else {
                Ok(Value::Nil)
            }
        }
        "truncate" => {
            // Truncate towards zero (like int() in Python)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("truncate expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Number(n.trunc() as i64))
        }
        "find" => {
            // Find index of first element matching predicate value
            // Simplified: (find lst value) returns index or #f
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "find expects 2 args: (find lst value)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let needle = &args[1];
            for (i, item) in list.iter().enumerate() {
                if value_eq(item, needle) {
                    return Ok(Value::Number(i as i64));
                }
            }
            Ok(Value::Bool(false))
        }
        "position" => {
            // Alias for find - returns index of element
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "position expects 2 args: (position lst value)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let needle = &args[1];
            for (i, item) in list.iter().enumerate() {
                if value_eq(item, needle) {
                    return Ok(Value::Number(i as i64));
                }
            }
            Ok(Value::Bool(false))
        }
        "contains?" => {
            // Check if list contains value
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "contains? expects 2 args: (contains? lst value)",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let needle = &args[1];
            let found = list.iter().any(|item| value_eq(item, needle));
            Ok(Value::Bool(found))
        }
        "min-list" => {
            // Find minimum element in list (requires comparable elements)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("min-list expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut min_val = list[0].clone();
            for item in &list[1..] {
                #[allow(clippy::collapsible_if)]
                if let (Ok(a), Ok(b)) = (expect_number(&min_val), expect_number(item)) {
                    if b.as_f64() < a.as_f64() {
                        min_val = item.clone();
                    }
                }
            }
            Ok(min_val)
        }
        "max-list" => {
            // Find maximum element in list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("max-list expects 1 arg: a list"));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut max_val = list[0].clone();
            for item in &list[1..] {
                #[allow(clippy::collapsible_if)]
                if let (Ok(a), Ok(b)) = (expect_number(&max_val), expect_number(item)) {
                    if b.as_f64() > a.as_f64() {
                        max_val = item.clone();
                    }
                }
            }
            Ok(max_val)
        }
        "inc" => {
            // Increment number by 1
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("inc expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i + 1)),
                Numeric::Float(f) => Ok(Value::Float(f + 1.0)),
            }
        }
        "dec" => {
            // Decrement number by 1
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("dec expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i - 1)),
                Numeric::Float(f) => Ok(Value::Float(f - 1.0)),
            }
        }
        "succ" => {
            // Successor (alias for inc)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("succ expects 1 arg: an integer"));
            }
            let n = expect_integer(&args[0])?;
            Ok(Value::Number(n + 1))
        }
        "pred" => {
            // Predecessor (alias for dec)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pred expects 1 arg: an integer"));
            }
            let n = expect_integer(&args[0])?;
            Ok(Value::Number(n - 1))
        }
        "head" => {
            // Alias for first/car
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("head expects 1 arg: a list"));
            }
            match &args[0] {
                Value::Pair(head, _) => Ok((**head).clone()),
                Value::Nil => Ok(Value::Nil),
                _ => Err(EvalError::TypeMismatch("head expects a list")),
            }
        }
        "tail" => {
            // Alias for rest/cdr
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("tail expects 1 arg: a list"));
            }
            match &args[0] {
                Value::Pair(_, tail) => Ok((**tail).clone()),
                Value::Nil => Ok(Value::Nil),
                _ => Err(EvalError::TypeMismatch("tail expects a list")),
            }
        }
        "atom?" => {
            // Check if value is an atom (not a pair and not nil)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("atom? expects 1 arg"));
            }
            let is_atom = !matches!(&args[0], Value::Pair(_, _) | Value::Nil);
            Ok(Value::Bool(is_atom))
        }
        "atomic?" => {
            // Alias for atom?
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("atomic? expects 1 arg"));
            }
            let is_atomic = !matches!(&args[0], Value::Pair(_, _) | Value::Nil);
            Ok(Value::Bool(is_atomic))
        }
        "not-null?" => {
            // Check if list is not empty/nil
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("not-null? expects 1 arg"));
            }
            let is_not_nil = !matches!(args[0], Value::Nil);
            Ok(Value::Bool(is_not_nil))
        }
        "proper-list?" => {
            // Check if value is a proper list (ends with nil)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("proper-list? expects 1 arg"));
            }
            let mut current = &args[0];
            loop {
                match current {
                    Value::Nil => return Ok(Value::Bool(true)),
                    Value::Pair(_, tail) => current = tail,
                    _ => return Ok(Value::Bool(false)),
                }
            }
        }
        "pair-list?" => {
            // Alias for proper-list?
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("pair-list? expects 1 arg"));
            }
            let mut current = &args[0];
            loop {
                match current {
                    Value::Nil => return Ok(Value::Bool(true)),
                    Value::Pair(_, tail) => current = tail,
                    _ => return Ok(Value::Bool(false)),
                }
            }
        }
        "list->symbols" => {
            // Convert list to list of symbols (mainly for type coercion)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list->symbols expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let symbols: Result<Vec<Value>, _> = list
                .into_iter()
                .map(|v| match v {
                    Value::Symbol(s) => Ok(Value::Symbol(s)),
                    _ => {
                        // Try to coerce to symbol
                        Ok(Value::Symbol(Symbol::intern(&value_to_display_string(&v))))
                    }
                })
                .collect();
            match symbols {
                Ok(syms) => Ok(list_from_values(&syms)),
                Err(e) => Err(e),
            }
        }
        "list->numbers" => {
            // Convert list to list of numbers (coerce strings/symbols if possible)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list->numbers expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let mut numbers = Vec::new();
            for v in list {
                match v {
                    Value::Number(n) => numbers.push(Value::Number(n)),
                    Value::Float(f) => numbers.push(Value::Float(f)),
                    Value::String(s) => {
                        // Try to parse string as number
                        if let Ok(n) = s.parse::<i64>() {
                            numbers.push(Value::Number(n));
                        } else if let Ok(f) = s.parse::<f64>() {
                            numbers.push(Value::Float(f));
                        } else {
                            return Err(EvalError::TypeMismatch(
                                "list->numbers: cannot convert to number",
                            ));
                        }
                    }
                    _ => return Err(EvalError::TypeMismatch("list->numbers: unsupported type")),
                }
            }
            Ok(list_from_values(&numbers))
        }
        "sq" => {
            // Square a number
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("sq expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i * i)),
                Numeric::Float(f) => Ok(Value::Float(f * f)),
            }
        }
        "cube" => {
            // Cube a number
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("cube expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i * i * i)),
                Numeric::Float(f) => Ok(Value::Float(f * f * f)),
            }
        }
        "double" => {
            // Double a number (multiply by 2)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("double expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i * 2)),
                Numeric::Float(f) => Ok(Value::Float(f * 2.0)),
            }
        }
        "halve" => {
            // Halve a number (divide by 2)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("halve expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(i / 2)),
                Numeric::Float(f) => Ok(Value::Float(f / 2.0)),
            }
        }
        "negate" => {
            // Negate a number (multiply by -1)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("negate expects 1 arg: a number"));
            }
            let n = expect_number(&args[0])?;
            match n {
                Numeric::Int(i) => Ok(Value::Number(-i)),
                Numeric::Float(f) => Ok(Value::Float(-f)),
            }
        }
        "zero" => {
            // Return zero of appropriate type
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("zero expects 0 args"));
            }
            Ok(Value::Number(0))
        }
        "one" => {
            // Return one of appropriate type
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("one expects 0 args"));
            }
            Ok(Value::Number(1))
        }
        "string-take" => {
            // Take first n characters from string
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-take expects 2 args: (string-take n str)",
                ));
            }
            let n = expect_usize(&args[0])?;
            let s = expect_string(&args[1])?;
            let result: String = s.chars().take(n).collect();
            Ok(Value::String(result))
        }
        "string-drop" => {
            // Drop first n characters from string
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-drop expects 2 args: (string-drop n str)",
                ));
            }
            let n = expect_usize(&args[0])?;
            let s = expect_string(&args[1])?;
            let result: String = s.chars().skip(n).collect();
            Ok(Value::String(result))
        }
        "starts-with?" => {
            // Check if string starts with prefix
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "starts-with? expects 2 args: (starts-with? str prefix)",
                ));
            }
            let s = expect_string(&args[0])?;
            let prefix = expect_string(&args[1])?;
            Ok(Value::Bool(s.starts_with(&prefix)))
        }
        "ends-with?" => {
            // Check if string ends with suffix
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "ends-with? expects 2 args: (ends-with? str suffix)",
                ));
            }
            let s = expect_string(&args[0])?;
            let suffix = expect_string(&args[1])?;
            Ok(Value::Bool(s.ends_with(&suffix)))
        }
        "substring?" => {
            // Check if first string contains second string
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "substring? expects 2 args: (substring? str substring)",
                ));
            }
            let s = expect_string(&args[0])?;
            let sub = expect_string(&args[1])?;
            Ok(Value::Bool(s.contains(&sub)))
        }
        // List manipulation and composition utilities
        "list-indices" => {
            // Get indices of a list: (list-indices '(a b c)) => (0 1 2)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-indices expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let indices: Vec<Value> = (0..list.len()).map(|i| Value::Number(i as i64)).collect();
            Ok(list_from_values(&indices))
        }
        "list-reverse-indices" => {
            // Get reverse indices: (list-reverse-indices '(a b c)) => (2 1 0)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-reverse-indices expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let indices: Vec<Value> = (0..list.len())
                .rev()
                .map(|i| Value::Number(i as i64))
                .collect();
            Ok(list_from_values(&indices))
        }
        "list-length-of" => {
            // Get list of lengths: (list-length-of '((1 2) (a b c))) => (2 3)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-length-of expects 1 arg: a list of lists",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let lengths: Vec<Value> = list
                .iter()
                .map(|v| {
                    let inner = list_to_vec(v).unwrap_or_default();
                    Value::Number(inner.len() as i64)
                })
                .collect();
            Ok(list_from_values(&lengths))
        }
        "list-split" => {
            // Split list into sublists of size n: (list-split 2 '(1 2 3 4 5)) => ((1 2) (3 4) (5))
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "list-split expects 2 args: (list-split n lst)",
                ));
            }
            let n = expect_usize(&args[0])?;
            if n == 0 {
                return Err(EvalError::TypeMismatch(
                    "list-split: chunk size must be > 0",
                ));
            }
            let list = list_to_vec(&args[1])?;
            let mut result = Vec::new();
            for chunk in list.chunks(n) {
                result.push(list_from_values(chunk));
            }
            Ok(list_from_values(&result))
        }
        "list-repeat-element" => {
            // Repeat each element n times: (list-repeat-element 2 '(a b c)) => (a a b b c c)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "list-repeat-element expects 2 args: (list-repeat-element n lst)",
                ));
            }
            let n = expect_usize(&args[0])?;
            let list = list_to_vec(&args[1])?;
            let mut result = Vec::new();
            for item in list {
                for _ in 0..n {
                    result.push(item.clone());
                }
            }
            Ok(list_from_values(&result))
        }
        "list-cycle" => {
            // Cycle through a list n times: (list-cycle 2 '(1 2)) => (1 2 1 2)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "list-cycle expects 2 args: (list-cycle n lst)",
                ));
            }
            let n = expect_usize(&args[0])?;
            let list = list_to_vec(&args[1])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut result = Vec::new();
            for _ in 0..n {
                result.extend(list.clone());
            }
            Ok(list_from_values(&result))
        }
        "list-intersperse" => {
            // Insert separator between elements: (list-intersperse 0 '(1 2 3)) => (1 0 2 0 3)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "list-intersperse expects 2 args: (list-intersperse sep lst)",
                ));
            }
            let sep = &args[0];
            let list = list_to_vec(&args[1])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut result = Vec::new();
            for (i, item) in list.iter().enumerate() {
                if i > 0 {
                    result.push(sep.clone());
                }
                result.push(item.clone());
            }
            Ok(list_from_values(&result))
        }
        "list-map-indexed" => {
            // Create list of (index value) pairs
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-map-indexed expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let mut result = Vec::new();
            for (i, item) in list.iter().enumerate() {
                let pair = Value::Pair(
                    Box::new(Value::Number(i as i64)),
                    Box::new(Value::Pair(Box::new(item.clone()), Box::new(Value::Nil))),
                );
                result.push(pair);
            }
            Ok(list_from_values(&result))
        }
        "list-enumerate" => {
            // Pair each element with its index: (list-enumerate '(a b c)) => ((0 a) (1 b) (2 c))
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-enumerate expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let mut result = Vec::new();
            for (i, item) in list.iter().enumerate() {
                let pair = Value::Pair(
                    Box::new(Value::Number(i as i64)),
                    Box::new(Value::Pair(Box::new(item.clone()), Box::new(Value::Nil))),
                );
                result.push(pair);
            }
            Ok(list_from_values(&result))
        }
        // Logical and conditional utilities
        "when" => {
            // When condition is true, return value, else return nil
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "when expects 2 args: (when cond val)",
                ));
            }
            let cond = &args[0];
            let val = &args[1];
            match cond {
                Value::Bool(false) | Value::Nil => Ok(Value::Nil),
                _ => Ok(val.clone()),
            }
        }
        "unless" => {
            // Unless condition is true, return value, else return nil (opposite of when)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "unless expects 2 args: (unless cond val)",
                ));
            }
            let cond = &args[0];
            let val = &args[1];
            match cond {
                Value::Bool(false) | Value::Nil => Ok(val.clone()),
                _ => Ok(Value::Nil),
            }
        }
        "implies" => {
            // Logical implication: (implies p q) => (or (not p) q)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "implies expects 2 args: (implies p q)",
                ));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(!p || q))
        }
        "iff" => {
            // Logical if-and-only-if: (iff p q) => (= p q)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("iff expects 2 args: (iff p q)"));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(p == q))
        }
        "nand" => {
            // Logical nand: (nand p q) => (not (and p q))
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("nand expects 2 args: (nand p q)"));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(!(p && q)))
        }
        "nor" => {
            // Logical nor: (nor p q) => (not (or p q))
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("nor expects 2 args: (nor p q)"));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(!(p || q)))
        }
        "if" => {
            // If-then-else: (if cond then-val else-val)
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "if expects 3 args: (if cond then else)",
                ));
            }
            let cond = &args[0];
            let then_val = &args[1];
            let else_val = &args[2];
            match cond {
                Value::Bool(false) | Value::Nil => Ok(else_val.clone()),
                _ => Ok(then_val.clone()),
            }
        }
        "cond-value" => {
            // Return first truthy value: (cond-value val1 val2 ...) => first non-#f, non-nil
            let mut result = Value::Nil;
            for v in args.iter() {
                match v {
                    Value::Bool(false) | Value::Nil => continue,
                    _ => {
                        result = v.clone();
                        break;
                    }
                }
            }
            Ok(result)
        }
        "coalesce" => {
            // Return first non-nil value
            for v in args.iter() {
                if !matches!(v, Value::Nil) {
                    return Ok(v.clone());
                }
            }
            Ok(Value::Nil)
        }
        "default-value" => {
            // Use second arg as default if first is nil/false
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "default-value expects 2 args: (default-value val default)",
                ));
            }
            match &args[0] {
                Value::Bool(false) | Value::Nil => Ok(args[1].clone()),
                v => Ok(v.clone()),
            }
        }
        "both?" => {
            // Check if both conditions are truthy
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("both? expects 2 args: (both? p q)"));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(p && q))
        }
        "either?" => {
            // Check if either condition is truthy
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "either? expects 2 args: (either? p q)",
                ));
            }
            let p = !matches!(&args[0], Value::Bool(false) | Value::Nil);
            let q = !matches!(&args[1], Value::Bool(false) | Value::Nil);
            Ok(Value::Bool(p || q))
        }
        // String conversion and manipulation utilities
        "string-words" => {
            // Split string into words: (string-words "hello world") => ("hello" "world")
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-words expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            let words: Vec<Value> = s
                .split_whitespace()
                .map(|word| Value::String(word.to_string()))
                .collect();
            Ok(list_from_values(&words))
        }
        "string-lines" => {
            // Split string into lines
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-lines expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            let lines: Vec<Value> = s
                .lines()
                .map(|line| Value::String(line.to_string()))
                .collect();
            Ok(list_from_values(&lines))
        }
        "string-join" => {
            // Join strings with separator: (string-join '("a" "b") "-") => "a-b"
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "string-join expects 2 args: (string-join strings separator)",
                ));
            }
            let strings = list_to_vec(&args[0])?;
            let sep = expect_string(&args[1])?;

            let mut parts = Vec::new();
            for s in strings {
                parts.push(value_to_display_string(&s));
            }
            Ok(Value::String(parts.join(&sep)))
        }
        "string-replace-all" => {
            // Replace all occurrences: (string-replace-all "hello" "l" "L") => "heLLo"
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "string-replace-all expects 3 args: (string-replace-all str from to)",
                ));
            }
            let s = expect_string(&args[0])?;
            let from = expect_string(&args[1])?;
            let to = expect_string(&args[2])?;

            Ok(Value::String(s.replace(&from, &to)))
        }
        "string-trim-start" => {
            // Trim leading whitespace
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-trim-start expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim_start().to_string()))
        }
        "string-trim-end" => {
            // Trim trailing whitespace
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-trim-end expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim_end().to_string()))
        }
        "string-trim-both" => {
            // Trim both ends
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-trim-both expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::String(s.trim().to_string()))
        }
        "string-ascii?" => {
            // Check if string is ASCII only
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-ascii? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(s.is_ascii()))
        }
        "string-numeric?" => {
            // Check if string contains only digits
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-numeric? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(
                !s.is_empty() && s.chars().all(|c| c.is_ascii_digit()),
            ))
        }
        "string-alphabetic?" => {
            // Check if string contains only letters
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-alphabetic? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(
                !s.is_empty() && s.chars().all(|c| c.is_alphabetic()),
            ))
        }
        "string-alphanumeric?" => {
            // Check if string contains only alphanumeric characters
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-alphanumeric? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(
                !s.is_empty() && s.chars().all(|c| c.is_alphanumeric()),
            ))
        }
        "string-whitespace?" => {
            // Check if string contains only whitespace
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-whitespace? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(
                !s.is_empty() && s.chars().all(|c| c.is_whitespace()),
            ))
        }
        "string-empty?" => {
            // Check if string is empty
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-empty? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bool(s.is_empty()))
        }
        // Vector/Array manipulation utilities
        "vec-copy" => {
            // Create a shallow copy of a vector
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("vec-copy expects 1 arg: a vector"));
            }
            match &args[0] {
                Value::Vector(items) => Ok(Value::Vector(items.clone())),
                _ => Err(EvalError::TypeMismatch("vec-copy expects a vector")),
            }
        }
        "vec-slice" => {
            // Get a slice of a vector: (vec-slice v start end)
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "vec-slice expects 3 args: (vec-slice vec start end)",
                ));
            }
            let start = expect_usize(&args[1])?;
            let end = expect_usize(&args[2])?;

            match &args[0] {
                Value::Vector(items) => {
                    let len = items.len();
                    let start = start.min(len);
                    let end = end.min(len);
                    if start > end {
                        return Ok(Value::Vector(Vec::new()));
                    }
                    Ok(Value::Vector(items[start..end].to_vec()))
                }
                _ => Err(EvalError::TypeMismatch("vec-slice expects a vector")),
            }
        }
        "vec-reverse" => {
            // Reverse a vector
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "vec-reverse expects 1 arg: a vector",
                ));
            }
            match &args[0] {
                Value::Vector(items) => {
                    let mut reversed = items.clone();
                    reversed.reverse();
                    Ok(Value::Vector(reversed))
                }
                _ => Err(EvalError::TypeMismatch("vec-reverse expects a vector")),
            }
        }
        "vec-sort" => {
            // Sort a vector of numbers/strings
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("vec-sort expects 1 arg: a vector"));
            }
            match &args[0] {
                Value::Vector(items) => {
                    let mut sorted = items.clone();
                    sorted.sort_by(|a, b| match (a, b) {
                        (Value::Number(x), Value::Number(y)) => x.cmp(y),
                        (Value::Float(x), Value::Float(y)) => {
                            if x < y {
                                std::cmp::Ordering::Less
                            } else if x > y {
                                std::cmp::Ordering::Greater
                            } else {
                                std::cmp::Ordering::Equal
                            }
                        }
                        (Value::String(x), Value::String(y)) => x.cmp(y),
                        _ => std::cmp::Ordering::Equal,
                    });
                    Ok(Value::Vector(sorted))
                }
                _ => Err(EvalError::TypeMismatch("vec-sort expects a vector")),
            }
        }
        "vec-contains?" => {
            // Check if vector contains a value
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "vec-contains? expects 2 args: (vec-contains? vec val)",
                ));
            }
            match &args[0] {
                Value::Vector(items) => {
                    for item in items {
                        if value_eq(item, &args[1]) {
                            return Ok(Value::Bool(true));
                        }
                    }
                    Ok(Value::Bool(false))
                }
                _ => Err(EvalError::TypeMismatch("vec-contains? expects a vector")),
            }
        }
        "vec-index-of" => {
            // Find index of value in vector
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "vec-index-of expects 2 args: (vec-index-of vec val)",
                ));
            }
            match &args[0] {
                Value::Vector(items) => {
                    for (i, item) in items.iter().enumerate() {
                        if value_eq(item, &args[1]) {
                            return Ok(Value::Number(i as i64));
                        }
                    }
                    Ok(Value::Number(-1))
                }
                _ => Err(EvalError::TypeMismatch("vec-index-of expects a vector")),
            }
        }
        // Type introspection and reflection
        "type-of" => {
            // Get type name of a value
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("type-of expects 1 arg: a value"));
            }
            let type_name = match &args[0] {
                Value::Number(_) => "number",
                Value::Float(_) => "float",
                Value::String(_) => "string",
                Value::Char(_) => "char",
                Value::Symbol(_) => "symbol",
                Value::Bool(_) => "bool",
                Value::Nil => "nil",
                Value::Pair(_, _) => "pair",
                Value::Vector(_) => "vector",
                Value::Bytevector(_) => "bytevector",
                Value::BigInt(_) => "bigint",
                Value::BigRational(_) => "bigint",
                _ => "unknown",
            };
            Ok(Value::Symbol(Symbol::intern(type_name)))
        }
        "is-number?" => {
            // Type check: is number
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("is-number? expects 1 arg: a value"));
            }
            Ok(Value::Bool(matches!(
                &args[0],
                Value::Number(_) | Value::Float(_) | Value::BigInt(_) | Value::BigRational(_)
            )))
        }
        "is-string?" => {
            // Type check: is string
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("is-string? expects 1 arg: a value"));
            }
            Ok(Value::Bool(matches!(&args[0], Value::String(_))))
        }
        "is-vector?" => {
            // Type check: is vector
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("is-vector? expects 1 arg: a value"));
            }
            Ok(Value::Bool(matches!(&args[0], Value::Vector(_))))
        }
        "is-list?" => {
            // Type check: is list (pair or nil)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("is-list? expects 1 arg: a value"));
            }
            Ok(Value::Bool(matches!(
                &args[0],
                Value::Pair(_, _) | Value::Nil
            )))
        }
        // Value comparison and equality utilities
        "equals?" => {
            // Deep equality check
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "equals? expects 2 args: (equals? a b)",
                ));
            }
            Ok(Value::Bool(value_eq(&args[0], &args[1])))
        }
        "not-equals?" => {
            // Deep inequality check
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "not-equals? expects 2 args: (not-equals? a b)",
                ));
            }
            Ok(Value::Bool(!value_eq(&args[0], &args[1])))
        }
        "identical?" => {
            // Check if two values are identical (same reference)
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "identical? expects 2 args: (identical? a b)",
                ));
            }
            // Since we don't have reference semantics in this impl,
            // use value equality as proxy
            Ok(Value::Bool(value_eq(&args[0], &args[1])))
        }
        "hash-value" => {
            // Simple hash of a value
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("hash-value expects 1 arg: a value"));
            }
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};

            let hash_code = match &args[0] {
                Value::Number(n) => {
                    let mut hasher = DefaultHasher::new();
                    n.hash(&mut hasher);
                    hasher.finish()
                }
                Value::String(s) => {
                    let mut hasher = DefaultHasher::new();
                    s.hash(&mut hasher);
                    hasher.finish()
                }
                Value::Symbol(s) => {
                    let mut hasher = DefaultHasher::new();
                    s.hash(&mut hasher);
                    hasher.finish()
                }
                Value::Float(f) => {
                    let mut hasher = DefaultHasher::new();
                    f.to_bits().hash(&mut hasher);
                    hasher.finish()
                }
                _ => {
                    // For other types, use a simple hash based on type discriminant
                    let mut hasher = DefaultHasher::new();
                    std::mem::size_of_val(&args[0]).hash(&mut hasher);
                    hasher.finish()
                }
            };

            Ok(Value::Number((hash_code % (i64::MAX as u64)) as i64))
        }
        // Utility and helper functions
        "constantly" => {
            // Return a function that always returns the given value
            // (constantly 42) returns a function that always returns 42
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("constantly expects 1 arg: a value"));
            }
            // Cannot implement closures at primitive level
            Err(EvalError::TypeMismatch(
                "constantly requires closure creation (not supported in Rust core)",
            ))
        }
        "list-count" => {
            // Count items in a list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("list-count expects 1 arg: a list"));
            }
            let count = list_to_vec(&args[0])?.len();
            Ok(Value::Number(count as i64))
        }
        "list-sum" => {
            // Sum numbers in a list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-sum expects 1 arg: a list of numbers",
                ));
            }
            let list = list_to_vec(&args[0])?;
            let mut sum: f64 = 0.0;
            let mut all_integers = true;

            for item in list {
                match expect_number(&item) {
                    Ok(Numeric::Int(n)) => sum += n as f64,
                    Ok(Numeric::Float(f)) => {
                        sum += f;
                        all_integers = false;
                    }
                    Err(_) => return Err(EvalError::TypeMismatch("list-sum: non-number in list")),
                }
            }

            if all_integers && sum.fract() == 0.0 && sum <= i64::MAX as f64 {
                Ok(Value::Number(sum as i64))
            } else {
                Ok(Value::Float(sum))
            }
        }
        "list-avg" => {
            // Average of numbers in a list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-avg expects 1 arg: a list of numbers",
                ));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Float(0.0));
            }

            let mut sum: f64 = 0.0;
            for item in &list {
                match expect_number(item) {
                    Ok(n) => sum += n.as_f64(),
                    Err(_) => return Err(EvalError::TypeMismatch("list-avg: non-number in list")),
                }
            }

            Ok(Value::Float(sum / list.len() as f64))
        }
        "list-min-value" => {
            // Minimum value in numeric list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-min-value expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }

            let mut min_val = list[0].clone();
            for item in &list[1..] {
                #[allow(clippy::collapsible_if)]
                if let (Ok(a), Ok(b)) = (expect_number(&min_val), expect_number(item)) {
                    if b.as_f64() < a.as_f64() {
                        min_val = item.clone();
                    }
                }
            }
            Ok(min_val)
        }
        "list-max-value" => {
            // Maximum value in numeric list
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "list-max-value expects 1 arg: a list",
                ));
            }
            let list = list_to_vec(&args[0])?;
            if list.is_empty() {
                return Ok(Value::Nil);
            }

            let mut max_val = list[0].clone();
            for item in &list[1..] {
                #[allow(clippy::collapsible_if)]
                if let (Ok(a), Ok(b)) = (expect_number(&max_val), expect_number(item)) {
                    if b.as_f64() > a.as_f64() {
                        max_val = item.clone();
                    }
                }
            }
            Ok(max_val)
        }
        "string-byte-length" => {
            // Get byte length of string (UTF-8)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-byte-length expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Number(s.len() as i64))
        }
        "string-char-length" => {
            // Get character length of string
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string-char-length expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Number(s.chars().count() as i64))
        }
        "value->string" => {
            // Convert any value to string representation
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "value->string expects 1 arg: a value",
                ));
            }
            Ok(Value::String(value_to_display_string(&args[0])))
        }
        "string->value?" => {
            // Try to parse string as a number
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string->value? expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;

            if let Ok(n) = s.parse::<i64>() {
                Ok(Value::Number(n))
            } else if let Ok(f) = s.parse::<f64>() {
                Ok(Value::Float(f))
            } else {
                Ok(Value::Nil)
            }
        }
        // Symbol and keyword utilities
        "symbol-upcase" => {
            // Convert symbol to uppercase
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "symbol-upcase expects 1 arg: a symbol",
                ));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Symbol(Symbol::intern(&s.as_str().to_uppercase()))),
                _ => Err(EvalError::TypeMismatch("symbol-upcase expects a symbol")),
            }
        }
        "symbol-downcase" => {
            // Convert symbol to lowercase
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "symbol-downcase expects 1 arg: a symbol",
                ));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Symbol(Symbol::intern(&s.as_str().to_lowercase()))),
                _ => Err(EvalError::TypeMismatch("symbol-downcase expects a symbol")),
            }
        }
        "symbol-length" => {
            // Get length of symbol name
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "symbol-length expects 1 arg: a symbol",
                ));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Number(s.as_str().len() as i64)),
                _ => Err(EvalError::TypeMismatch("symbol-length expects a symbol")),
            }
        }
        "symbol-concat" => {
            // Concatenate symbols into one
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::Symbol(s) => result.push_str(&s.as_str()),
                    Value::String(s) => result.push_str(s),
                    _ => {
                        return Err(EvalError::TypeMismatch(
                            "symbol-concat expects symbols or strings",
                        ));
                    }
                }
            }
            Ok(Value::Symbol(Symbol::intern(&result)))
        }
        // Bytevector utilities
        "bytevector-length" => {
            // Get length of bytevector
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "bytevector-length expects 1 arg: a bytevector",
                ));
            }
            match &args[0] {
                Value::Bytevector(bytes) => Ok(Value::Number(bytes.len() as i64)),
                _ => Err(EvalError::TypeMismatch(
                    "bytevector-length expects a bytevector",
                )),
            }
        }
        "bytevector-ref" => {
            // Get byte at index
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "bytevector-ref expects 2 args: (bytevector-ref bv index)",
                ));
            }
            let idx = expect_usize(&args[1])?;
            match &args[0] {
                Value::Bytevector(bytes) => {
                    if idx < bytes.len() {
                        Ok(Value::Number(bytes[idx] as i64))
                    } else {
                        Err(EvalError::TypeMismatch("bytevector index out of bounds"))
                    }
                }
                _ => Err(EvalError::TypeMismatch(
                    "bytevector-ref expects a bytevector",
                )),
            }
        }
        "bytevector->string" => {
            // Convert bytevector to string (UTF-8)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "bytevector->string expects 1 arg: a bytevector",
                ));
            }
            match &args[0] {
                Value::Bytevector(bytes) => match String::from_utf8(bytes.clone()) {
                    Ok(s) => Ok(Value::String(s)),
                    Err(_) => Err(EvalError::TypeMismatch("bytevector->string: invalid UTF-8")),
                },
                _ => Err(EvalError::TypeMismatch(
                    "bytevector->string expects a bytevector",
                )),
            }
        }
        "string->bytevector" => {
            // Convert string to bytevector (UTF-8)
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "string->bytevector expects 1 arg: a string",
                ));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Bytevector(s.into_bytes()))
        }
        // Association list utilities
        "alist-get" => {
            // Get value from association list
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "alist-get expects 2 args: (alist-get key alist)",
                ));
            }
            let key = &args[0];
            let alist = list_to_vec(&args[1])?;

            for item in alist {
                if let Value::Pair(k, v) = &item
                    && value_eq(k, key)
                {
                    return Ok((**v).clone());
                }
            }
            Ok(Value::Nil)
        }
        "alist-set" => {
            // Create new alist with updated key-value
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "alist-set expects 3 args: (alist-set key value alist)",
                ));
            }
            let key = &args[0];
            let value = &args[1];
            let alist = list_to_vec(&args[2])?;

            let mut result = Vec::new();
            let mut found = false;

            for item in alist {
                if let Value::Pair(k, _) = &item {
                    if value_eq(k, key) {
                        result.push(Value::Pair(Box::new(key.clone()), Box::new(value.clone())));
                        found = true;
                    } else {
                        result.push(item);
                    }
                } else {
                    result.push(item);
                }
            }

            if !found {
                result.push(Value::Pair(Box::new(key.clone()), Box::new(value.clone())));
            }

            Ok(list_from_values(&result))
        }
        "alist-keys" => {
            // Get all keys from alist
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "alist-keys expects 1 arg: an alist",
                ));
            }
            let alist = list_to_vec(&args[0])?;
            let mut keys = Vec::new();

            for item in alist {
                if let Value::Pair(k, _) = item {
                    keys.push((*k).clone());
                }
            }

            Ok(list_from_values(&keys))
        }
        "alist-values" => {
            // Get all values from alist
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch(
                    "alist-values expects 1 arg: an alist",
                ));
            }
            let alist = list_to_vec(&args[0])?;
            let mut values = Vec::new();

            for item in alist {
                if let Value::Pair(_, v) = item {
                    values.push((*v).clone());
                }
            }

            Ok(list_from_values(&values))
        }

        // ============================================================
        // File I/O Primitives (for forum support)
        // ============================================================
        "file-exists?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("file-exists? expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            Ok(Value::Bool(std::path::Path::new(&path).exists()))
        }
        "file-directory?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("file-directory? expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            Ok(Value::Bool(std::path::Path::new(&path).is_dir()))
        }
        "read-file" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("read-file expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::read_to_string(&path) {
                Ok(contents) => Ok(Value::String(contents)),
                Err(e) => Err(EvalError::IOError(format!("read-file: {}: {}", path, e))),
            }
        }
        "read-file-bytes" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("read-file-bytes expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::read(&path) {
                Ok(bytes) => Ok(Value::Bytevector(bytes)),
                Err(e) => Err(EvalError::IOError(format!(
                    "read-file-bytes: {}: {}",
                    path, e
                ))),
            }
        }
        "write-file" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "write-file expects 2 args: path, content",
                ));
            }
            let path = expect_string(&args[0])?;
            let content = expect_string(&args[1])?;
            match std::fs::write(&path, &content) {
                Ok(()) => Ok(Value::Nil),
                Err(e) => Err(EvalError::IOError(format!("write-file: {}: {}", path, e))),
            }
        }
        "write-file-bytes" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "write-file-bytes expects 2 args: path, bytes",
                ));
            }
            let path = expect_string(&args[0])?;
            let bytes = expect_bytevector(&args[1])?;
            match std::fs::write(&path, bytes) {
                Ok(()) => Ok(Value::Nil),
                Err(e) => Err(EvalError::IOError(format!(
                    "write-file-bytes: {}: {}",
                    path, e
                ))),
            }
        }
        "append-file" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "append-file expects 2 args: path, content",
                ));
            }
            let path = expect_string(&args[0])?;
            let content = expect_string(&args[1])?;
            use std::io::Write;
            let mut file = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
                .map_err(|e| EvalError::IOError(format!("append-file: {}: {}", path, e)))?;
            file.write_all(content.as_bytes())
                .map_err(|e| EvalError::IOError(format!("append-file: {}: {}", path, e)))?;
            Ok(Value::Nil)
        }
        "mkdir" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("mkdir expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::create_dir(&path) {
                Ok(()) => Ok(Value::Nil),
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => Ok(Value::Nil),
                Err(e) => Err(EvalError::IOError(format!("mkdir: {}: {}", path, e))),
            }
        }
        "mkdir-p" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("mkdir-p expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::create_dir_all(&path) {
                Ok(()) => Ok(Value::Nil),
                Err(e) => Err(EvalError::IOError(format!("mkdir-p: {}: {}", path, e))),
            }
        }
        "delete-file" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("delete-file expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::remove_file(&path) {
                Ok(()) => Ok(Value::Nil),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Value::Nil),
                Err(e) => Err(EvalError::IOError(format!("delete-file: {}: {}", path, e))),
            }
        }
        "directory-list" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("directory-list expects 1 arg"));
            }
            let path = expect_string(&args[0])?;
            match std::fs::read_dir(&path) {
                Ok(entries) => {
                    let mut names = Vec::new();
                    for e in entries.flatten() {
                        if let Some(name) = e.file_name().to_str() {
                            names.push(Value::String(name.to_string()));
                        }
                    }
                    Ok(list_from_values(&names))
                }
                Err(e) => Err(EvalError::IOError(format!(
                    "directory-list: {}: {}",
                    path, e
                ))),
            }
        }
        "current-time-seconds" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch(
                    "current-time-seconds expects 0 args",
                ));
            }
            use std::time::{SystemTime, UNIX_EPOCH};
            let secs = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs() as i64)
                .unwrap_or(0);
            Ok(Value::Number(secs))
        }
        "current-time-millis" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch(
                    "current-time-millis expects 0 args",
                ));
            }
            use std::time::{SystemTime, UNIX_EPOCH};
            let millis = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as i64)
                .unwrap_or(0);
            Ok(Value::Number(millis))
        }
        "current-timestamp" => {
            // Return ISO 8601 timestamp string
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("current-timestamp expects 0 args"));
            }
            use std::time::{SystemTime, UNIX_EPOCH};
            let secs = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0);
            // Simple ISO 8601 format (no chrono dependency)
            let days_since_1970 = secs / 86400;
            let secs_in_day = secs % 86400;
            let hours = secs_in_day / 3600;
            let mins = (secs_in_day % 3600) / 60;
            let secs_rem = secs_in_day % 60;
            // Approximate year/month/day (good enough for forum posts)
            let mut year = 1970i64;
            let mut remaining_days = days_since_1970 as i64;
            loop {
                let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                    366
                } else {
                    365
                };
                if remaining_days < days_in_year {
                    break;
                }
                remaining_days -= days_in_year;
                year += 1;
            }
            let is_leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
            let days_in_months = if is_leap {
                [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            } else {
                [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            };
            let mut month = 1;
            for &dim in &days_in_months {
                if remaining_days < dim as i64 {
                    break;
                }
                remaining_days -= dim as i64;
                month += 1;
            }
            let day = remaining_days + 1;
            Ok(Value::String(format!(
                "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
                year, month, day, hours, mins, secs_rem
            )))
        }

        // ============================================================
        // Forum Commands (for Rust daemon feature parity)
        // ============================================================
        "forum-channels" => {
            // List available forum channels
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("forum-channels expects 0 args"));
            }
            let heads_dir = ".store/heads";
            match std::fs::read_dir(heads_dir) {
                Ok(entries) => {
                    let mut channels = Vec::new();
                    for entry in entries.flatten() {
                        if let Some(name) = entry.file_name().to_str()
                            && let Some(channel) = name.strip_suffix(".head")
                        {
                            channels.push(Value::Symbol(Symbol::from(channel)));
                        }
                    }
                    Ok(list_from_values(&channels))
                }
                Err(_) => Ok(Value::Nil),
            }
        }
        "forum-digest" => {
            // Simple digest: list recent posts
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("forum-digest expects 0 args"));
            }
            // Print a simple digest header
            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║                    THE FOLD — FORUM DIGEST                   ║");
            println!("╚══════════════════════════════════════════════════════════════╝\n");

            // List channels with basic info
            let heads_dir = ".store/heads";
            if let Ok(entries) = std::fs::read_dir(heads_dir) {
                println!("Available channels:");
                for entry in entries.flatten() {
                    if let Some(name) = entry.file_name().to_str()
                        && let Some(channel) = name.strip_suffix(".head")
                    {
                        println!("  #{}", channel);
                    }
                }
            }
            println!("\nUse (browse 'channel) to view posts.");
            Ok(Value::Nil)
        }
        // Aliases for Scheme compatibility
        "channels" => {
            // Alias for forum-channels
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("channels expects 0 args"));
            }
            let heads_dir = ".store/heads";
            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║                    AVAILABLE CHANNELS                        ║");
            println!("╚══════════════════════════════════════════════════════════════╝\n");
            if let Ok(entries) = std::fs::read_dir(heads_dir) {
                for entry in entries.flatten() {
                    if let Some(name) = entry.file_name().to_str()
                        && let Some(channel) = name.strip_suffix(".head")
                    {
                        println!("  #{}", channel);
                    }
                }
            }
            Ok(Value::Nil)
        }
        "digest" => {
            // Alias for forum-digest
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("digest expects 0 args"));
            }
            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║                    THE FOLD — FORUM DIGEST                   ║");
            println!("╚══════════════════════════════════════════════════════════════╝\n");
            let heads_dir = ".store/heads";
            if let Ok(entries) = std::fs::read_dir(heads_dir) {
                println!("Available channels:");
                for entry in entries.flatten() {
                    if let Some(name) = entry.file_name().to_str()
                        && let Some(channel) = name.strip_suffix(".head")
                    {
                        println!("  #{}", channel);
                    }
                }
            }
            println!("\nUse (browse 'channel) to view posts.");
            println!("Use (chat \"message\") to post to chat.");
            Ok(Value::Nil)
        }

        // ============================================================
        // Session Commands (hi, who, bye)
        // ============================================================
        "hi" => {
            // (hi 'model-tier 'name) or (hi 'model-tier 'name "message")
            if args.len() < 2 || args.len() > 3 {
                return Err(EvalError::TypeMismatch(
                    "hi expects 2-3 args: model-tier, name, [message]",
                ));
            }
            let model_tier = expect_symbol(&args[0])?;
            let name = expect_symbol(&args[1])?;
            let message = if args.len() == 3 {
                Some(expect_string(&args[2])?)
            } else {
                None
            };

            // Map model to role
            let model_str = model_tier.as_str();
            let role = model_to_role(&model_str).ok_or(EvalError::TypeMismatch(
                "hi: invalid tier. Use 'opus, 'sonnet, or 'haiku",
            ))?;

            // Get or generate session ID
            let session_id =
                get_current_session_id().unwrap_or_else(|| format!("cli-{}", std::process::id()));

            // Write session file
            let name_str = name.as_str();
            if let Err(e) =
                write_session_file(&session_id, role, &model_str, &name_str)
            {
                return Err(EvalError::IOError(format!(
                    "hi: failed to write session: {}",
                    e
                )));
            }

            // Print welcome message
            println!("Logged in as {} ({}).", name, role);
            if let Some(msg) = message {
                println!("Message: {}", msg);
            }
            println!("Use (digest) to see forum activity, (help) for commands.");

            Ok(Value::Nil)
        }
        "who" => {
            // (who) - show current session info
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("who expects 0 args"));
            }

            let session_id =
                get_current_session_id().unwrap_or_else(|| format!("cli-{}", std::process::id()));

            if let Some(session) = read_session_file(&session_id) {
                // Extract name and tier from alist
                let name = alist_get(&session, "name")
                    .map(|v| format_value_simple(&v))
                    .unwrap_or_else(|| "unknown".to_string());
                let tier = alist_get(&session, "tier")
                    .map(|v| format_value_simple(&v))
                    .unwrap_or_else(|| "unknown".to_string());
                println!("Logged in as: {} ({})", name, tier);
                println!("Session: {}", session_id);
            } else {
                println!("No active session. Use (hi 'tier 'name) to login.");
            }

            Ok(Value::Nil)
        }
        "bye" => {
            // (bye) - logout and clear session
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("bye expects 0 args"));
            }

            let session_id =
                get_current_session_id().unwrap_or_else(|| format!("cli-{}", std::process::id()));

            if let Some(session) = read_session_file(&session_id) {
                let name = alist_get(&session, "name")
                    .map(|v| format_value_simple(&v))
                    .unwrap_or_else(|| "unknown".to_string());

                if let Err(e) = delete_session_file(&session_id) {
                    return Err(EvalError::IOError(format!(
                        "bye: failed to clear session: {}",
                        e
                    )));
                }
                println!("Goodbye, {}. Session cleared.", name);
            } else {
                println!("No active session.");
            }

            Ok(Value::Nil)
        }

        // ============================================================
        // Chat Command
        // ============================================================
        "chat" => {
            // (chat "message") - post to chat channel
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("chat expects 1 arg: message"));
            }
            let message = expect_string(&args[0])?;

            let session_id =
                get_current_session_id().unwrap_or_else(|| format!("cli-{}", std::process::id()));

            // Get session info
            let session = read_session_file(&session_id).ok_or(EvalError::TypeMismatch(
                "chat: no active session. Use (hi 'tier 'name) first",
            ))?;

            let author = alist_get(&session, "name")
                .map(|v| format_value_simple(&v))
                .unwrap_or_else(|| "anonymous".to_string());
            let tier = alist_get(&session, "tier")
                .map(|v| format_value_simple(&v))
                .unwrap_or_else(|| "player".to_string());

            // Get current timestamp
            let timestamp = current_iso_timestamp();

            // Create post block
            let body = format!(
                "((author . {}) (tier . {}) (timestamp . \"{}\") (channel . chat) (body . \"{}\"))",
                author,
                tier,
                timestamp,
                escape_string(&message)
            );

            // Hash the post
            let hash = sha256(body.as_bytes());

            // Read current chat head
            let head_path = ".store/heads/chat.head";
            let prev_head = std::fs::read_to_string(head_path).ok();

            // Store the block (with sharding by first 2 hex chars)
            let hash_hex = bytes_to_hex(&hash);
            let shard = if hash_hex.len() >= 2 {
                &hash_hex[..2]
            } else {
                &hash_hex
            };
            let shard_dir = format!(".store/objects/{}", shard);
            std::fs::create_dir_all(&shard_dir).ok();
            let object_path = format!("{}/{}", shard_dir, hash_hex);
            if let Err(e) = std::fs::write(&object_path, &body) {
                return Err(EvalError::IOError(format!(
                    "chat: failed to store post: {}",
                    e
                )));
            }

            // Build refs (link to previous head if exists)
            // Note: For full chain integrity, refs should be embedded in the block
            let _refs_str = if let Some(prev) = prev_head {
                format!("(refs . (\"{}\"))", prev.trim())
            } else {
                "(refs . ())".to_string()
            };

            // Update head
            std::fs::create_dir_all(".store/heads").ok();
            if let Err(e) = std::fs::write(head_path, &hash_hex) {
                return Err(EvalError::IOError(format!(
                    "chat: failed to update head: {}",
                    e
                )));
            }

            println!("{}: {}", author, message);
            Ok(Value::Nil)
        }

        // ============================================================
        // Browse Command
        // ============================================================
        "browse" => {
            // (browse 'channel) or (browse 'channel n)
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::TypeMismatch(
                    "browse expects 1-2 args: channel, [count]",
                ));
            }
            let channel = expect_symbol(&args[0])?;
            let count = if args.len() == 2 {
                expect_integer(&args[1])? as usize
            } else {
                5
            };

            let head_path = format!(".store/heads/{}.head", channel);
            let head_hex = match std::fs::read_to_string(&head_path) {
                Ok(h) => h.trim().to_string(),
                Err(_) => {
                    println!("Channel #{} has no posts.", channel);
                    return Ok(Value::Nil);
                }
            };

            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║  #{:<56} ║", channel);
            println!("╚══════════════════════════════════════════════════════════════╝\n");

            // Walk the chain and display posts
            let mut current_hash = head_hex;
            let mut displayed = 0;

            while displayed < count && !current_hash.is_empty() {
                // Objects are stored with sharding: .store/objects/XX/XXXX...
                let shard = if current_hash.len() >= 2 {
                    &current_hash[..2]
                } else {
                    &current_hash
                };
                let object_path = format!(".store/objects/{}/{}", shard, current_hash);
                // Read as bytes since blocks may have binary refs
                match std::fs::read(&object_path) {
                    Ok(bytes) => {
                        // Convert to lossy string (ignores invalid UTF-8)
                        let content = String::from_utf8_lossy(&bytes);
                        // Parse and display post
                        if let Some(post) = parse_post_content(&content) {
                            let author = post
                                .get("author")
                                .cloned()
                                .unwrap_or_else(|| "?".to_string());
                            let tier = post.get("tier").cloned().unwrap_or_else(|| "?".to_string());
                            let timestamp = post
                                .get("timestamp")
                                .cloned()
                                .unwrap_or_else(|| "?".to_string());
                            let body = post.get("body").cloned().unwrap_or_default();
                            let hash_short = if current_hash.len() >= 8 {
                                &current_hash[..8]
                            } else {
                                &current_hash
                            };

                            let tier_badge = match tier.as_str() {
                                "shepherd" => "🐑",
                                "builder" => "🔨",
                                "player" => "🎮",
                                _ => "?",
                            };

                            println!(
                                "  [{}] {} {} ({}): {}",
                                format_timestamp_short(&timestamp),
                                tier_badge,
                                author,
                                hash_short,
                                truncate_str(&body, 60)
                            );
                            displayed += 1;

                            // Get previous ref
                            if let Some(refs) = post.get("refs") {
                                current_hash = refs.clone();
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }

            if displayed == 0 {
                println!("  (no posts)");
            }

            Ok(Value::Nil)
        }

        // ============================================================
        // msg Command - Post to Forum Channel with Title
        // ============================================================
        "msg" => {
            // (msg 'channel "title" "body") - post to forum channel
            if args.len() != 3 {
                return Err(EvalError::TypeMismatch(
                    "msg expects 3 args: channel, title, body",
                ));
            }
            let channel = expect_symbol(&args[0])?;
            let title = expect_string(&args[1])?;
            let body_text = expect_string(&args[2])?;

            let session_id =
                get_current_session_id().unwrap_or_else(|| format!("cli-{}", std::process::id()));

            // Get session info
            let session = read_session_file(&session_id).ok_or(EvalError::TypeMismatch(
                "msg: no active session. Use (hi 'tier 'name) first",
            ))?;

            let author = alist_get(&session, "name")
                .map(|v| format_value_simple(&v))
                .unwrap_or_else(|| "anonymous".to_string());
            let tier = alist_get(&session, "tier")
                .map(|v| format_value_simple(&v))
                .unwrap_or_else(|| "player".to_string());

            // Get current timestamp
            let timestamp = current_iso_timestamp();

            // Format body with title (matching Scheme behavior)
            let formatted_body = format!("## {}\n\n{}", title, body_text);

            // Create post block with title field
            let block_content = format!(
                "((author . {}) (tier . {}) (timestamp . \"{}\") (channel . {}) (title . \"{}\") (body . \"{}\"))",
                author,
                tier,
                timestamp,
                channel,
                escape_string(&title),
                escape_string(&formatted_body)
            );

            // Hash the post
            let hash = sha256(block_content.as_bytes());

            // Read current channel head
            let head_path = format!(".store/heads/{}.head", channel);
            let prev_head = std::fs::read_to_string(&head_path).ok();

            // Store the block (with sharding by first 2 hex chars)
            let hash_hex = bytes_to_hex(&hash);
            let shard = if hash_hex.len() >= 2 {
                &hash_hex[..2]
            } else {
                &hash_hex
            };
            let shard_dir = format!(".store/objects/{}", shard);
            std::fs::create_dir_all(&shard_dir).ok();
            let object_path = format!("{}/{}", shard_dir, hash_hex);
            if let Err(e) = std::fs::write(&object_path, &block_content) {
                return Err(EvalError::IOError(format!(
                    "msg: failed to store post: {}",
                    e
                )));
            }

            // Build refs (link to previous head if exists)
            let _refs_str = if let Some(prev) = prev_head {
                format!("(refs . (\"{}\"))", prev.trim())
            } else {
                "(refs . ())".to_string()
            };

            // Update head
            std::fs::create_dir_all(".store/heads").ok();
            if let Err(e) = std::fs::write(&head_path, &hash_hex) {
                return Err(EvalError::IOError(format!(
                    "msg: failed to update head: {}",
                    e
                )));
            }

            println!("Posted to #{}: {}", channel, title);
            println!("Hash: {}", hash_hex);
            Ok(Value::Nil)
        }

        // ============================================================
        // digest-posts Command - Show Recent Posts (Excluding Chat)
        // ============================================================
        "digest-posts" => {
            // (digest-posts) or (digest-posts n) - show recent posts without chat
            if args.len() > 1 {
                return Err(EvalError::TypeMismatch(
                    "digest-posts expects 0-1 args: [count]",
                ));
            }
            let count = if args.len() == 1 {
                expect_integer(&args[0])? as usize
            } else {
                10
            };

            println!("\n╔══════════════════════════════════════════════════════════════╗");
            println!("║                    THE FOLD — FORUM DIGEST                   ║");
            println!("╚══════════════════════════════════════════════════════════════╝\n");

            println!("┌─────────────────────────────────────────────────────────────┐");
            println!("│ RECENT POSTS (non-chat)                                     │");
            println!("└─────────────────────────────────────────────────────────────┘");

            // Collect posts from all non-chat/non-system channels
            let heads_dir = ".store/heads";
            let mut all_posts: Vec<(String, String, std::collections::HashMap<String, String>)> =
                Vec::new(); // (channel, hash, post_data)

            if let Ok(entries) = std::fs::read_dir(heads_dir) {
                for entry in entries.filter_map(|e| e.ok()) {
                    let filename = entry.file_name().to_string_lossy().to_string();
                    if filename.ends_with(".head") {
                        let channel_name = filename.trim_end_matches(".head").to_string();
                        // Skip chat and system channels
                        if channel_name == "chat" || channel_name == "system" {
                            continue;
                        }
                        // Read up to `count` posts from this channel
                        if let Ok(head_hex) = std::fs::read_to_string(entry.path()) {
                            let mut current_hash = head_hex.trim().to_string();
                            let mut channel_posts = 0;

                            while channel_posts < count && !current_hash.is_empty() {
                                let shard = if current_hash.len() >= 2 {
                                    &current_hash[..2]
                                } else {
                                    &current_hash
                                };
                                let object_path =
                                    format!(".store/objects/{}/{}", shard, current_hash);

                                if let Ok(bytes) = std::fs::read(&object_path) {
                                    let content = String::from_utf8_lossy(&bytes);
                                    if let Some(mut post) = parse_post_content(&content) {
                                        post.insert("_channel".to_string(), channel_name.clone());
                                        all_posts.push((
                                            channel_name.clone(),
                                            current_hash.clone(),
                                            post.clone(),
                                        ));
                                        channel_posts += 1;

                                        // Get previous ref
                                        if let Some(refs) = post.get("refs") {
                                            current_hash = refs.clone();
                                        } else {
                                            break;
                                        }
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            // Sort by timestamp (descending)
            all_posts.sort_by(|a, b| {
                let ts_a = a.2.get("timestamp").cloned().unwrap_or_default();
                let ts_b = b.2.get("timestamp").cloned().unwrap_or_default();
                ts_b.cmp(&ts_a) // Descending
            });

            // Display top `count` posts
            if all_posts.is_empty() {
                println!("  (no posts yet)");
            } else {
                for (channel, hash, post) in all_posts.iter().take(count) {
                    let author = post
                        .get("author")
                        .cloned()
                        .unwrap_or_else(|| "?".to_string());
                    let title = post.get("title");
                    let body = post.get("body").cloned().unwrap_or_default();
                    let hash_short = if hash.len() >= 6 { &hash[..6] } else { hash };

                    let display_body = if let Some(t) = title {
                        format!("[{}] {}", t, body)
                    } else {
                        body
                    };

                    println!(
                        "  #{} | {} [{}]: {}",
                        channel,
                        author,
                        hash_short,
                        truncate_str(&display_body, 50)
                    );
                }
            }
            println!();

            Ok(Value::Nil)
        }

        // Port operations
        "open-input-string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("open-input-string expects 1 arg"));
            }
            let content = expect_string(&args[0])?;
            use crate::fabric::Port;
            Ok(Value::Port(Rc::new(RefCell::new(Port::StringInput {
                content,
                position: 0,
            }))))
        }
        "open-output-string" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("open-output-string expects 0 args"));
            }
            use crate::fabric::Port;
            Ok(Value::Port(Rc::new(RefCell::new(Port::StringOutput {
                buffer: String::new(),
            }))))
        }
        "get-output-string" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("get-output-string expects 1 arg"));
            }
            match &args[0] {
                Value::Port(port_ref) => {
                    let port = port_ref.borrow();
                    match &*port {
                        crate::fabric::Port::StringOutput { buffer } => {
                            Ok(Value::String(buffer.clone()))
                        }
                        _ => Err(EvalError::TypeMismatch("expected output port")),
                    }
                }
                _ => Err(EvalError::TypeMismatch("expected port")),
            }
        }
        "port?" => type_predicate(args, |v| matches!(v, Value::Port(_))),
        "input-port?" => type_predicate(args, |v| {
            matches!(v, Value::Port(port_ref) if matches!(&*port_ref.borrow(), crate::fabric::Port::StringInput { .. }))
        }),
        "output-port?" => type_predicate(args, |v| {
            matches!(v, Value::Port(port_ref) if matches!(&*port_ref.borrow(), crate::fabric::Port::StringOutput { .. }))
        }),
        "read-char" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("read-char expects 1 arg"));
            }
            match &args[0] {
                Value::Port(port_ref) => {
                    let mut port = port_ref.borrow_mut();
                    match &mut *port {
                        crate::fabric::Port::StringInput { content, position } => {
                            if *position >= content.len() {
                                // Return EOF object (special symbol)
                                Ok(Value::Symbol(Symbol::intern("#<eof>")))
                            } else {
                                let ch = content[*position..].chars().next().unwrap();
                                *position += ch.len_utf8();
                                Ok(Value::Char(ch))
                            }
                        }
                        _ => Err(EvalError::TypeMismatch("expected input port")),
                    }
                }
                _ => Err(EvalError::TypeMismatch("expected port")),
            }
        }
        "peek-char" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("peek-char expects 1 arg"));
            }
            match &args[0] {
                Value::Port(port_ref) => {
                    let port = port_ref.borrow();
                    match &*port {
                        crate::fabric::Port::StringInput { content, position } => {
                            if *position >= content.len() {
                                // Return EOF object (special symbol)
                                Ok(Value::Symbol(Symbol::intern("#<eof>")))
                            } else {
                                let ch = content[*position..].chars().next().unwrap();
                                Ok(Value::Char(ch))
                            }
                        }
                        _ => Err(EvalError::TypeMismatch("expected input port")),
                    }
                }
                _ => Err(EvalError::TypeMismatch("expected port")),
            }
        }
        "write-char" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("write-char expects 2 args"));
            }
            let ch = match &args[0] {
                Value::Char(c) => *c,
                _ => return Err(EvalError::TypeMismatch("expected char")),
            };
            match &args[1] {
                Value::Port(port_ref) => {
                    let mut port = port_ref.borrow_mut();
                    match &mut *port {
                        crate::fabric::Port::StringOutput { buffer } => {
                            buffer.push(ch);
                            Ok(Value::Nil)
                        }
                        _ => Err(EvalError::TypeMismatch("expected output port")),
                    }
                }
                _ => Err(EvalError::TypeMismatch("expected port")),
            }
        }
        "eof-object?" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("eof-object? expects 1 arg"));
            }
            Ok(Value::Bool(matches!(
                &args[0],
                Value::Symbol(sym) if sym.as_str() == "#<eof>"
            )))
        }
        "eof-object" => {
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("eof-object expects 0 args"));
            }
            Ok(Value::Symbol(Symbol::intern("#<eof>")))
        }

        _ => Err(EvalError::UnknownPrimitive(op.clone())),
    }
}

#[derive(Debug, Clone, Copy)]
enum Numeric {
    Int(i64),
    Float(f64),
}

impl Numeric {
    fn as_f64(self) -> f64 {
        match self {
            Numeric::Int(n) => n as f64,
            Numeric::Float(n) => n,
        }
    }
}

fn numeric_to_value(num: Numeric) -> Value {
    match num {
        Numeric::Int(n) => Value::Number(n),
        Numeric::Float(n) => Value::Float(n),
    }
}

fn expect_number(value: &Value) -> Result<Numeric, EvalError> {
    match value {
        Value::Number(n) => Ok(Numeric::Int(*n)),
        Value::Float(n) => Ok(Numeric::Float(*n)),
        _ => Err(EvalError::TypeMismatch("expected number")),
    }
}

fn expect_integer(value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Number(n) => Ok(*n),
        _ => Err(EvalError::TypeMismatch("expected integer")),
    }
}

fn fold_numeric<FInt, FFloat>(
    args: &[Value],
    init_int: i64,
    init_float: f64,
    op_int: FInt,
    op_float: FFloat,
) -> Result<Value, EvalError>
where
    FInt: Fn(i64, i64) -> i64,
    FFloat: Fn(f64, f64) -> f64,
{
    let mut has_float = false;
    let mut int_acc = init_int;
    let mut float_acc = init_float;
    for arg in args {
        match expect_number(arg)? {
            Numeric::Int(n) => {
                if has_float {
                    float_acc = op_float(float_acc, n as f64);
                } else {
                    int_acc = op_int(int_acc, n);
                }
            }
            Numeric::Float(n) => {
                if !has_float {
                    has_float = true;
                    float_acc = int_acc as f64;
                }
                float_acc = op_float(float_acc, n);
            }
        }
    }
    if has_float {
        Ok(Value::Float(float_acc))
    } else {
        Ok(Value::Number(int_acc))
    }
}

fn reduce_numeric<FInt, FFloat>(
    first: Numeric,
    rest: &[Value],
    op_int: FInt,
    op_float: FFloat,
) -> Result<Value, EvalError>
where
    FInt: Fn(i64, i64) -> i64,
    FFloat: Fn(f64, f64) -> f64,
{
    let mut has_float = matches!(first, Numeric::Float(_));
    let mut int_acc = match first {
        Numeric::Int(n) => n,
        Numeric::Float(_) => 0,
    };
    let mut float_acc = first.as_f64();
    for arg in rest {
        match expect_number(arg)? {
            Numeric::Int(n) => {
                if has_float {
                    float_acc = op_float(float_acc, n as f64);
                } else {
                    int_acc = op_int(int_acc, n);
                }
            }
            Numeric::Float(n) => {
                if !has_float {
                    has_float = true;
                    float_acc = int_acc as f64;
                }
                float_acc = op_float(float_acc, n);
            }
        }
    }
    if has_float {
        Ok(Value::Float(float_acc))
    } else {
        Ok(Value::Number(int_acc))
    }
}

fn negate_numeric(num: Numeric) -> Numeric {
    match num {
        Numeric::Int(n) => Numeric::Int(-n),
        Numeric::Float(n) => Numeric::Float(-n),
    }
}

fn abs_numeric(num: Numeric) -> Numeric {
    match num {
        Numeric::Int(n) => Numeric::Int(n.abs()),
        Numeric::Float(n) => Numeric::Float(n.abs()),
    }
}

fn expect_usize(value: &Value) -> Result<usize, EvalError> {
    let n = expect_integer(value)?;
    if n < 0 {
        return Err(EvalError::TypeMismatch("expected non-negative index"));
    }
    Ok(n as usize)
}

fn expect_u32(value: &Value) -> Result<u32, EvalError> {
    let n = expect_integer(value)?;
    u32::try_from(n).map_err(|_| EvalError::TypeMismatch("expected u32"))
}

fn expect_bigint(value: &Value) -> Result<&BigInt, EvalError> {
    match value {
        Value::BigInt(n) => Ok(n),
        _ => Err(EvalError::TypeMismatch("expected bigint")),
    }
}

fn expect_rational(value: &Value) -> Result<&BigRational, EvalError> {
    match value {
        Value::BigRational(r) => Ok(r),
        _ => Err(EvalError::TypeMismatch("expected rational")),
    }
}

fn to_bigint(value: &Value) -> Result<BigInt, EvalError> {
    match value {
        Value::BigInt(n) => Ok(n.clone()),
        Value::Number(n) => Ok(BigInt::from(*n)),
        _ => Err(EvalError::TypeMismatch("expected bigint or number")),
    }
}

fn bigint_binary<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&BigInt, &BigInt) -> BigInt,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("bigint op expects 2 args"));
    }
    let a = expect_bigint(&args[0])?;
    let b = expect_bigint(&args[1])?;
    Ok(Value::BigInt(f(a, b)))
}

fn bigint_cmp<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&BigInt, &BigInt) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("bigint comparison expects 2 args"));
    }
    let a = expect_bigint(&args[0])?;
    let b = expect_bigint(&args[1])?;
    Ok(Value::Bool(f(a, b)))
}

fn rational_binary<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&BigRational, &BigRational) -> BigRational,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("rational op expects 2 args"));
    }
    let a = expect_rational(&args[0])?;
    let b = expect_rational(&args[1])?;
    Ok(Value::BigRational(f(a, b)))
}

fn rational_cmp<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&BigRational, &BigRational) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch(
            "rational comparison expects 2 args",
        ));
    }
    let a = expect_rational(&args[0])?;
    let b = expect_rational(&args[1])?;
    Ok(Value::Bool(f(a, b)))
}

fn expect_symbol(value: &Value) -> Result<Symbol, EvalError> {
    match value {
        Value::Symbol(sym) => Ok(sym.clone()),
        _ => Err(EvalError::TypeMismatch("expected symbol")),
    }
}

fn expect_string(value: &Value) -> Result<String, EvalError> {
    match value {
        Value::String(s) => Ok(s.clone()),
        _ => Err(EvalError::TypeMismatch("expected string")),
    }
}

fn expect_char(value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        _ => Err(EvalError::TypeMismatch("expected char")),
    }
}

fn expect_bytevector(value: &Value) -> Result<Vec<u8>, EvalError> {
    match value {
        Value::Bytevector(bytes) => Ok(bytes.clone()),
        _ => Err(EvalError::TypeMismatch("expected bytevector")),
    }
}

fn expect_vector(value: &Value) -> Result<&[Value], EvalError> {
    match value {
        Value::Vector(values) => Ok(values),
        _ => Err(EvalError::TypeMismatch("expected vector")),
    }
}

fn expect_block(args: &[Value]) -> Result<&Block, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::TypeMismatch("block op expects 1 arg"));
    }
    match &args[0] {
        Value::Block(block) => Ok(block),
        _ => Err(EvalError::TypeMismatch("expected block")),
    }
}

fn to_address(value: &Value) -> Result<Address, EvalError> {
    match value {
        Value::Address(address) => Ok(*address),
        Value::Bytevector(bytes) => {
            if bytes.len() != ADDRESS_SIZE {
                return Err(EvalError::TypeMismatch("expected address bytes"));
            }
            let array: [u8; ADDRESS_SIZE] = bytes
                .as_slice()
                .try_into()
                .map_err(|_| EvalError::TypeMismatch("expected address bytes"))?;
            Ok(Address::from(array))
        }
        _ => Err(EvalError::TypeMismatch("expected address")),
    }
}

fn cmp_numbers<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(f64, f64) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("comparison expects 2 args"));
    }
    let left = expect_number(&args[0])?.as_f64();
    let right = expect_number(&args[1])?.as_f64();
    Ok(Value::Bool(f(left, right)))
}

fn unary_number_pred<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(f64) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::TypeMismatch("numeric predicate expects 1 arg"));
    }
    let n = expect_number(&args[0])?.as_f64();
    Ok(Value::Bool(f(n)))
}

fn binary_number<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> i64,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("binary numeric op expects 2 args"));
    }
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Number(f(a, b)))
}

fn shift_number<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(i64, u32) -> i64,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("shift expects 2 args"));
    }
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    let shift = u32::try_from(b).map_err(|_| EvalError::TypeMismatch("invalid shift"))?;
    Ok(Value::Number(f(a, shift)))
}

fn type_predicate<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::TypeMismatch("predicate expects 1 arg"));
    }
    Ok(Value::Bool(f(&args[0])))
}

fn binary_string_cmp<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("string comparison expects 2 args"));
    }
    let a = expect_string(&args[0])?;
    let b = expect_string(&args[1])?;
    Ok(Value::Bool(f(&a, &b)))
}

fn binary_char_cmp<F>(args: &[Value], f: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() != 2 {
        return Err(EvalError::TypeMismatch("char comparison expects 2 args"));
    }
    let a = expect_char(&args[0])?;
    let b = expect_char(&args[1])?;
    Ok(Value::Bool(f(a, b)))
}

fn list_from_values(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}

fn list_to_vec(list: &Value) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = list;
    loop {
        match current {
            Value::Nil => return Ok(items),
            Value::Pair(head, tail) => {
                items.push((**head).clone());
                current = tail;
            }
            _ => return Err(EvalError::TypeMismatch("expected proper list")),
        }
    }
}

fn is_list(value: &Value) -> bool {
    list_to_vec(value).is_ok()
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn value_to_display_string(value: &Value) -> String {
    match value {
        Value::Number(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Symbol(s) => s.to_string(),
        Value::Bool(b) => if *b { "#t" } else { "#f" }.to_string(),
        Value::Nil => "()".to_string(),
        Value::Char(c) => format!("#\\{}", c),
        Value::BigInt(n) => n.to_string(),
        Value::BigRational(r) => r.to_string(),
        Value::Bytevector(bv) => format!("#u8(...{})", bv.len()),
        Value::Address(_) => "#<addr>".to_string(),
        Value::Vector(_) => "#(...)".to_string(),
        Value::Pair(_, _) => {
            let items = list_to_vec(value).unwrap_or_default();
            let strs: Vec<String> = items.iter().map(value_to_display_string).collect();
            format!("({})", strs.join(" "))
        }
        Value::Block(b) => format!("#<block:{}>", b.tag),
        Value::Closure(_) => "#<fn>".to_string(),
        Value::Primitive(name) => format!("#<primitive {}>", name),
        Value::Hashtable(_) => "#<hashtable>".to_string(),
        Value::Port(_) => "#<port>".to_string(),
    }
}

fn value_to_write_string(value: &Value) -> String {
    match value {
        Value::Number(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
        Value::Symbol(s) => s.to_string(),
        Value::Bool(b) => if *b { "#t" } else { "#f" }.to_string(),
        Value::Nil => "()".to_string(),
        Value::Char(c) => format!("#\\{}", c),
        Value::BigInt(n) => n.to_string(),
        Value::BigRational(r) => r.to_string(),
        Value::Bytevector(bv) => {
            let bytes: Vec<String> = bv.iter().map(|b| b.to_string()).collect();
            format!("#u8({})", bytes.join(" "))
        }
        Value::Address(addr) => format!("#<addr:{}>", address_to_hex(addr)),
        Value::Vector(items) => {
            let strs: Vec<String> = items.iter().map(value_to_write_string).collect();
            format!("#({})", strs.join(" "))
        }
        Value::Pair(_, _) => {
            let items = list_to_vec(value).unwrap_or_default();
            let strs: Vec<String> = items.iter().map(value_to_write_string).collect();
            format!("({})", strs.join(" "))
        }
        Value::Block(b) => format!("#<block:{}>", b.tag),
        Value::Closure(_) => "#<fn>".to_string(),
        Value::Primitive(name) => format!("#<primitive {}>", name),
        Value::Hashtable(_) => "#<hashtable>".to_string(),
        Value::Port(_) => "#<port>".to_string(),
    }
}

fn value_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Number(a), Value::Float(b)) => (*a as f64) == *b,
        (Value::Float(a), Value::Number(b)) => *a == (*b as f64),
        (Value::BigInt(a), Value::BigInt(b)) => bigint::bigint_eq(a, b),
        (Value::BigInt(a), Value::Number(b)) => bigint::bigint_eq(a, &BigInt::from(*b)),
        (Value::Number(a), Value::BigInt(b)) => bigint::bigint_eq(&BigInt::from(*a), b),
        (Value::BigRational(a), Value::BigRational(b)) => bigrational::bigrational_eq(a, b),
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Bytevector(a), Value::Bytevector(b)) => a == b,
        (Value::Address(a), Value::Address(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, b1), Value::Pair(a2, b2)) => value_eq(a1, a2) && value_eq(b1, b2),
        (Value::Vector(a), Value::Vector(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| value_eq(x, y))
        }
        (Value::Block(a), Value::Block(b)) => {
            a.tag == b.tag && a.payload == b.payload && a.refs == b.refs
        }
        _ => false,
    }
}

// ============================================================
// Session and Forum Helper Functions
// ============================================================

/// Get value from alist by key
fn alist_get(alist: &Value, key: &str) -> Option<Value> {
    let mut current = alist;
    while let Value::Pair(car, cdr) = current {
        if let Value::Pair(k, v) = car.as_ref()
            && let Value::Symbol(sym) = k.as_ref()
            && sym.as_str() == key
        {
            return Some(v.as_ref().clone());
        }
        current = cdr.as_ref();
    }
    None
}

/// Simple value formatter for session display
fn format_value_simple(value: &Value) -> String {
    match value {
        Value::Symbol(s) => s.to_string(),
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => if *b { "#t" } else { "#f" }.to_string(),
        _ => "?".to_string(),
    }
}

/// Generate current ISO 8601 timestamp
fn current_iso_timestamp() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    let days_since_1970 = secs / 86400;
    let secs_in_day = secs % 86400;
    let hours = secs_in_day / 3600;
    let mins = (secs_in_day % 3600) / 60;
    let secs_rem = secs_in_day % 60;

    let mut year = 1970i64;
    let mut remaining_days = days_since_1970 as i64;
    loop {
        let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
            366
        } else {
            365
        };
        if remaining_days < days_in_year {
            break;
        }
        remaining_days -= days_in_year;
        year += 1;
    }
    let is_leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    let days_in_months = if is_leap {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };
    let mut month = 1;
    for &dim in &days_in_months {
        if remaining_days < dim as i64 {
            break;
        }
        remaining_days -= dim as i64;
        month += 1;
    }
    let day = remaining_days + 1;

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year, month, day, hours, mins, secs_rem
    )
}

/// Escape string for S-expression
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Convert bytes to hex string
fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

/// Parse post content (Block format: tag + payload + refs)
fn parse_post_content(content: &str) -> Option<std::collections::HashMap<String, String>> {
    // Block format may have: "   forum-post<byte>   ((author ...) ...)<binary refs>"
    // We need to find the alist starting with "(("
    let content = content.trim();

    // Find the alist within the content
    let alist_start = content.find("((")?;
    let alist_content = &content[alist_start..];

    // Find the end of the alist (matching closing parens)
    let mut depth = 0;
    let mut alist_end = 0;
    for (i, c) in alist_content.chars().enumerate() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    alist_end = i + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    if alist_end == 0 {
        return None;
    }

    let content = &alist_content[..alist_end];

    let mut result = std::collections::HashMap::new();

    // Simple parser for ((key . value) (key . value) ...) format
    let mut pos = 1; // Skip opening paren
    let chars: Vec<char> = content.chars().collect();

    while pos < chars.len() {
        // Skip whitespace
        while pos < chars.len() && chars[pos].is_whitespace() {
            pos += 1;
        }

        if pos >= chars.len() || chars[pos] == ')' {
            break;
        }

        if chars[pos] != '(' {
            pos += 1;
            continue;
        }

        // Parse (key . value)
        pos += 1; // Skip '('

        // Read key
        let mut key = String::new();
        while pos < chars.len() && !chars[pos].is_whitespace() && chars[pos] != '.' {
            key.push(chars[pos]);
            pos += 1;
        }

        // Skip to '.'
        while pos < chars.len() && chars[pos] != '.' {
            pos += 1;
        }
        pos += 1; // Skip '.'

        // Skip whitespace
        while pos < chars.len() && chars[pos].is_whitespace() {
            pos += 1;
        }

        // Read value
        let mut value = String::new();
        let in_string = pos < chars.len() && chars[pos] == '"';
        if in_string {
            pos += 1; // Skip opening quote
            while pos < chars.len() {
                if chars[pos] == '\\' && pos + 1 < chars.len() {
                    pos += 1;
                    match chars[pos] {
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        c => value.push(c),
                    }
                } else if chars[pos] == '"' {
                    pos += 1;
                    break;
                } else {
                    value.push(chars[pos]);
                }
                pos += 1;
            }
        } else {
            // Read until ')'
            while pos < chars.len() && chars[pos] != ')' && !chars[pos].is_whitespace() {
                value.push(chars[pos]);
                pos += 1;
            }
        }

        // Skip to closing ')'
        while pos < chars.len() && chars[pos] != ')' {
            pos += 1;
        }
        pos += 1; // Skip ')'

        if !key.is_empty() {
            result.insert(key, value);
        }
    }

    Some(result)
}

/// Format timestamp for display (HH:MM)
fn format_timestamp_short(timestamp: &str) -> String {
    if timestamp.len() >= 16 {
        timestamp[11..16].to_string()
    } else {
        timestamp.to_string()
    }
}

/// Truncate string to max length
fn truncate_str(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len.saturating_sub(3)])
    }
}
