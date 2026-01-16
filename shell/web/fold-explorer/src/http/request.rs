//! HTTP request parsing.

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read};
use std::net::TcpStream;

/// An HTTP request.
#[derive(Debug)]
pub struct Request {
    pub method: String,
    pub path: String,
    pub query: HashMap<String, String>,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

impl Request {
    /// Parse an HTTP request from a TCP stream.
    pub fn parse(stream: &mut TcpStream) -> Option<Self> {
        let mut reader = BufReader::new(stream.try_clone().ok()?);

        // Read request line
        let mut request_line = String::new();
        reader.read_line(&mut request_line).ok()?;
        let parts: Vec<&str> = request_line.trim().split_whitespace().collect();
        if parts.len() < 2 {
            return None;
        }

        let method = parts[0].to_string();
        let full_path = parts[1];

        // Parse path and query string
        let (path, query) = if let Some(idx) = full_path.find('?') {
            let path = full_path[..idx].to_string();
            let query_str = &full_path[idx + 1..];
            let query = parse_query_string(query_str);
            (path, query)
        } else {
            (full_path.to_string(), HashMap::new())
        };

        // Read headers
        let mut headers = HashMap::new();
        let mut content_length = 0;
        loop {
            let mut line = String::new();
            reader.read_line(&mut line).ok()?;
            let line = line.trim();
            if line.is_empty() {
                break;
            }
            if let Some(idx) = line.find(':') {
                let key = line[..idx].trim().to_lowercase();
                let value = line[idx + 1..].trim().to_string();
                if key == "content-length" {
                    content_length = value.parse().unwrap_or(0);
                }
                headers.insert(key, value);
            }
        }

        // Read body if present
        let mut body = vec![0u8; content_length];
        if content_length > 0 {
            reader.read_exact(&mut body).ok()?;
        }

        Some(Self {
            method,
            path,
            query,
            headers,
            body,
        })
    }

    /// Get a query parameter by name.
    pub fn query_param(&self, name: &str) -> Option<&str> {
        self.query.get(name).map(|s| s.as_str())
    }

    /// Get a query parameter as usize.
    pub fn query_param_usize(&self, name: &str, default: usize) -> usize {
        self.query
            .get(name)
            .and_then(|s| s.parse().ok())
            .unwrap_or(default)
    }
}

/// Parse a query string into key-value pairs.
fn parse_query_string(s: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for pair in s.split('&') {
        if let Some(idx) = pair.find('=') {
            let key = url_decode(&pair[..idx]);
            let value = url_decode(&pair[idx + 1..]);
            map.insert(key, value);
        }
    }
    map
}

/// URL decode a string.
fn url_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            // Try to read two hex digits
            let hex: String = chars.by_ref().take(2).collect();
            if hex.len() == 2 {
                if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                    result.push(byte as char);
                    continue;
                }
            }
            result.push('%');
            result.push_str(&hex);
        } else if c == '+' {
            result.push(' ');
        } else {
            result.push(c);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_decode() {
        assert_eq!(url_decode("hello"), "hello");
        assert_eq!(url_decode("hello%20world"), "hello world");
        assert_eq!(url_decode("hello+world"), "hello world");
    }

    #[test]
    fn test_parse_query_string() {
        let query = parse_query_string("a=1&b=hello%20world");
        assert_eq!(query.get("a"), Some(&"1".to_string()));
        assert_eq!(query.get("b"), Some(&"hello world".to_string()));
    }
}
