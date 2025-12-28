use std::{
    convert::TryInto,
    sync::{Mutex, OnceLock},
};

use crate::fabric::{
    address::{Address, ADDRESS_SIZE},
    block::Block,
    cas::{address_to_hex, hash_block, hex_to_address, Store},
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
        // Arithmetic
        "add" => {
            fold_numeric(args, 0, 0.0, |a, b| a + b, |a, b| a + b)
        }
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
        "mul" => {
            fold_numeric(args, 1, 1.0, |a, b| a * b, |a, b| a * b)
        }
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
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::TypeMismatch("expt expects 2 args"));
            }
            let base = expect_number(&args[0])?;
            let exp = expect_number(&args[1])?;
            match (base, exp) {
                (Numeric::Int(b), Numeric::Int(e)) if e >= 0 => {
                    Ok(Value::Number(b.pow(e as u32)))
                }
                _ => Ok(Value::Float(base.as_f64().powf(exp.as_f64()))),
            }
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

        // Comparison
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
        "number?" => type_predicate(args, |v| matches!(v, Value::Number(_) | Value::Float(_))),
        "integer?" => type_predicate(args, |v| matches!(v, Value::Number(_))),
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
            Ok(Value::String(std::iter::repeat(ch).take(len).collect()))
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
            let code = u32::try_from(code).map_err(|_| EvalError::TypeMismatch("invalid char code"))?;
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
            let s = String::from_utf8(bytes).map_err(|_| EvalError::TypeMismatch("invalid utf8"))?;
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

fn expect_block<'a>(args: &'a [Value]) -> Result<&'a Block, EvalError> {
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
            let array: [u8; ADDRESS_SIZE] = bytes.as_slice().try_into().map_err(|_| {
                EvalError::TypeMismatch("expected address bytes")
            })?;
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
    match list_to_vec(value) {
        Ok(_) => true,
        Err(_) => false,
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn value_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Number(a), Value::Float(b)) => (*a as f64) == *b,
        (Value::Float(a), Value::Number(b)) => *a == (*b as f64),
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
