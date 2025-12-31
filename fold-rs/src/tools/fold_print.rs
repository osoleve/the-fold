use crate::fabric::hex::bytes_to_hex;
use crate::fabric::value::Value;

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Number(n) => n.to_string(),
        Value::Float(n) => n.to_string(),
        Value::BigInt(n) => n.to_string(),
        Value::BigRational(r) => {
            if r.is_integer() {
                r.numer().to_string()
            } else {
                format!("{}/{}", r.numer(), r.denom())
            }
        }
        Value::String(s) => format_string(s),
        Value::Symbol(sym) => sym.as_str(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::Char(ch) => format_char(*ch),
        Value::Bytevector(bytes) => format_bytevector(bytes),
        Value::Address(address) => format!("#<address {}>", bytes_to_hex(address.as_bytes())),
        Value::Pair(_, _) => format_pair(value),
        Value::Vector(values) => format_vector(values),
        Value::Closure(closure) => {
            if closure.params.is_empty() {
                "#<closure/0>".to_string()
            } else if closure.params.len() <= 3 {
                // Show params for small arities
                let params: Vec<String> = closure.params.iter().map(|s| s.as_str()).collect();
                format!("#<closure ({})>", params.join(" "))
            } else {
                // Just show arity for larger closures
                format!("#<closure/{}>", closure.params.len())
            }
        }
        Value::Primitive(op) => format!("#<primitive {}>", op.as_str()),
        Value::Block(block) => format!("#<block {}>", block.tag.as_str()),
        Value::Hashtable(ht) => {
            let size = ht.borrow().len();
            format!("#<hashtable size={}>", size)
        }
        Value::Port(port_ref) => {
            let port = port_ref.borrow();
            match &*port {
                crate::fabric::Port::StringInput { .. } => "#<input-port>".to_string(),
                crate::fabric::Port::StringOutput { .. } => "#<output-port>".to_string(),
            }
        }
        Value::Condition { kind, message, irritants } => {
            if irritants.is_empty() {
                format!("#<condition {} \"{}\">", kind.as_str(), message)
            } else {
                let irr_str: Vec<String> = irritants.iter().map(format_value).collect();
                format!("#<condition {} \"{}\" [{}]>", kind.as_str(), message, irr_str.join(" "))
            }
        }
        Value::Nil => "()".to_string(),
    }
}

fn format_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn format_char(ch: char) -> String {
    match ch {
        '\n' => "#\\newline".to_string(),
        '\t' => "#\\tab".to_string(),
        '\r' => "#\\return".to_string(),
        ' ' => "#\\space".to_string(),
        _ => format!("#\\{ch}"),
    }
}

fn format_vector(values: &[Value]) -> String {
    let mut out = String::from("#(");
    for (idx, value) in values.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        out.push_str(&format_value(value));
    }
    out.push(')');
    out
}

fn format_bytevector(bytes: &[u8]) -> String {
    let mut out = String::from("#u8(");
    for (idx, b) in bytes.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        out.push_str(&b.to_string());
    }
    out.push(')');
    out
}

fn format_pair(value: &Value) -> String {
    let mut out = String::new();
    out.push('(');
    let mut first = true;
    let mut current = value;
    loop {
        match current {
            Value::Pair(car, cdr) => {
                if !first {
                    out.push(' ');
                }
                out.push_str(&format_value(car));
                current = cdr;
                first = false;
            }
            Value::Nil => {
                out.push(')');
                break;
            }
            other => {
                out.push_str(" . ");
                out.push_str(&format_value(other));
                out.push(')');
                break;
            }
        }
    }
    out
}
