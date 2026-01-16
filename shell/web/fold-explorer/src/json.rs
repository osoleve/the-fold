//! Hand-rolled JSON serialization (no dependencies).
//!
//! Provides a simple builder pattern for constructing JSON values.

/// Escape a string for JSON output.
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// A JSON value builder.
pub struct Json;

impl Json {
    /// Create a JSON object from key-value pairs.
    pub fn object(fields: &[(&str, String)]) -> String {
        let inner: Vec<String> = fields
            .iter()
            .map(|(k, v)| format!("\"{}\":{}", escape_string(k), v))
            .collect();
        format!("{{{}}}", inner.join(","))
    }

    /// Create a JSON array from values.
    pub fn array(items: &[String]) -> String {
        format!("[{}]", items.join(","))
    }

    /// Create a JSON string value.
    pub fn string(s: &str) -> String {
        format!("\"{}\"", escape_string(s))
    }

    /// Create a JSON number value.
    pub fn number<T: std::fmt::Display>(n: T) -> String {
        n.to_string()
    }

    /// Create a JSON boolean value.
    pub fn bool(b: bool) -> String {
        if b {
            "true".to_string()
        } else {
            "false".to_string()
        }
    }

    /// Create a JSON null value.
    pub fn null() -> String {
        "null".to_string()
    }
}

/// Trait for types that can be serialized to JSON.
pub trait ToJson {
    fn to_json(&self) -> String;
}

impl ToJson for String {
    fn to_json(&self) -> String {
        Json::string(self)
    }
}

impl ToJson for &str {
    fn to_json(&self) -> String {
        Json::string(self)
    }
}

impl ToJson for bool {
    fn to_json(&self) -> String {
        Json::bool(*self)
    }
}

impl ToJson for usize {
    fn to_json(&self) -> String {
        Json::number(*self)
    }
}

impl ToJson for u64 {
    fn to_json(&self) -> String {
        Json::number(*self)
    }
}

impl ToJson for i64 {
    fn to_json(&self) -> String {
        Json::number(*self)
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    fn to_json(&self) -> String {
        let items: Vec<String> = self.iter().map(|x| x.to_json()).collect();
        Json::array(&items)
    }
}

impl<T: ToJson> ToJson for Option<T> {
    fn to_json(&self) -> String {
        match self {
            Some(v) => v.to_json(),
            None => Json::null(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_string() {
        assert_eq!(escape_string("hello"), "hello");
        assert_eq!(escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(escape_string("quote\"here"), "quote\\\"here");
    }

    #[test]
    fn test_json_object() {
        let obj = Json::object(&[("name", Json::string("test")), ("count", Json::number(42))]);
        assert_eq!(obj, r#"{"name":"test","count":42}"#);
    }

    #[test]
    fn test_json_array() {
        let arr = Json::array(&[Json::number(1), Json::number(2), Json::number(3)]);
        assert_eq!(arr, "[1,2,3]");
    }
}
