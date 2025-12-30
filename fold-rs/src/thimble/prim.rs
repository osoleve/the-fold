use std::{
    convert::TryInto,
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

pub fn apply_prim(op: &Symbol, args: &[Value]) -> Result<Value, EvalError> {
    match op.as_str() {
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
        "log" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("log expects 1 arg"));
            }
            let n = expect_number(&args[0])?.as_f64();
            Ok(Value::Float(n.ln()))
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
        "assq" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("assq expects 2 args"));
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
                        _ => return Err(EvalError::TypeMismatch("assq expects alist")),
                    },
                    _ => return Err(EvalError::TypeMismatch("assq expects alist")),
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
            Ok(Value::String(sym))
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::TypeMismatch("string->symbol expects 1 arg"));
            }
            let s = expect_string(&args[0])?;
            Ok(Value::Symbol(s))
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
        "who" => {
            // Return session info - for now just indicate no active session
            // The session system is Scheme-specific, so in Rust we just return a placeholder
            if !args.is_empty() {
                return Err(EvalError::TypeMismatch("who expects 0 args"));
            }
            Ok(Value::String(
                "Rust daemon session (use Scheme daemon for full session management)".to_string(),
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
            Ok(Value::Symbol(format!("g{}", id)))
        }
        "filter" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch(
                    "filter expects 2 args: (filter pred lst)",
                ));
            }
            // Note: filter requires closure evaluation - not supported at prim level
            Err(EvalError::TypeMismatch(
                "filter requires closure evaluation (not yet supported in Rust core)",
            ))
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
