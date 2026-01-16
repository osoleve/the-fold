//! HTTP response building with security headers.

use std::collections::HashMap;

/// An HTTP response.
pub struct Response {
    pub status: u16,
    pub status_text: String,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

impl Response {
    /// Create a new response with the given status.
    pub fn new(status: u16, status_text: &str) -> Self {
        let mut headers = HashMap::new();
        // Security headers (always included)
        headers.insert("X-Content-Type-Options".to_string(), "nosniff".to_string());
        headers.insert("X-Frame-Options".to_string(), "DENY".to_string());

        Self {
            status,
            status_text: status_text.to_string(),
            headers,
            body: Vec::new(),
        }
    }

    /// Create a 200 OK response.
    pub fn ok() -> Self {
        Self::new(200, "OK")
    }

    /// Create a 404 Not Found response.
    pub fn not_found() -> Self {
        let mut resp = Self::new(404, "Not Found");
        resp.json(r#"{"error":"not found"}"#);
        resp
    }

    /// Create a 400 Bad Request response.
    pub fn bad_request(message: &str) -> Self {
        let mut resp = Self::new(400, "Bad Request");
        resp.json(&format!(r#"{{"error":"{}"}}"#, message));
        resp
    }

    /// Create a 500 Internal Server Error response.
    pub fn internal_error(message: &str) -> Self {
        let mut resp = Self::new(500, "Internal Server Error");
        resp.json(&format!(r#"{{"error":"{}"}}"#, message));
        resp
    }

    /// Set the body as JSON.
    pub fn json(&mut self, body: &str) -> &mut Self {
        self.headers
            .insert("Content-Type".to_string(), "application/json".to_string());
        self.body = body.as_bytes().to_vec();
        self
    }

    /// Set the body as HTML.
    pub fn html(&mut self, body: &str) -> &mut Self {
        self.headers.insert(
            "Content-Type".to_string(),
            "text/html; charset=utf-8".to_string(),
        );
        // Add CSP header for HTML
        self.headers.insert(
            "Content-Security-Policy".to_string(),
            "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'".to_string(),
        );
        self.body = body.as_bytes().to_vec();
        self
    }

    /// Set the body as plain text.
    pub fn text(&mut self, body: &str) -> &mut Self {
        self.headers.insert(
            "Content-Type".to_string(),
            "text/plain; charset=utf-8".to_string(),
        );
        self.body = body.as_bytes().to_vec();
        self
    }

    /// Set the body as CSS.
    pub fn css(&mut self, body: &str) -> &mut Self {
        self.headers.insert(
            "Content-Type".to_string(),
            "text/css; charset=utf-8".to_string(),
        );
        self.body = body.as_bytes().to_vec();
        self
    }

    /// Set the body as JavaScript.
    pub fn javascript(&mut self, body: &str) -> &mut Self {
        self.headers.insert(
            "Content-Type".to_string(),
            "application/javascript; charset=utf-8".to_string(),
        );
        self.body = body.as_bytes().to_vec();
        self
    }

    /// Set the body as raw binary (for payloads).
    /// SECURITY: Always uses application/octet-stream to prevent XSS.
    pub fn binary(&mut self, body: Vec<u8>) -> &mut Self {
        self.headers.insert(
            "Content-Type".to_string(),
            "application/octet-stream".to_string(),
        );
        self.body = body;
        self
    }

    /// Set a header.
    pub fn header(&mut self, key: &str, value: &str) -> &mut Self {
        self.headers.insert(key.to_string(), value.to_string());
        self
    }

    /// Convert to HTTP response bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = format!("HTTP/1.1 {} {}\r\n", self.status, self.status_text);

        // Add Content-Length
        result.push_str(&format!("Content-Length: {}\r\n", self.body.len()));

        // Add all headers
        for (key, value) in &self.headers {
            result.push_str(&format!("{}: {}\r\n", key, value));
        }

        result.push_str("\r\n");

        let mut bytes = result.into_bytes();
        bytes.extend_from_slice(&self.body);
        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_response_ok() {
        let mut resp = Response::ok();
        resp.json(r#"{"status":"ok"}"#);
        let bytes = resp.to_bytes();
        let text = String::from_utf8_lossy(&bytes);
        assert!(text.contains("HTTP/1.1 200 OK"));
        assert!(text.contains("Content-Type: application/json"));
        assert!(text.contains("X-Content-Type-Options: nosniff"));
    }

    #[test]
    fn test_response_binary_security() {
        let mut resp = Response::ok();
        resp.binary(vec![0x3c, 0x73, 0x63, 0x72, 0x69, 0x70, 0x74, 0x3e]); // <script>
        assert_eq!(
            resp.headers.get("Content-Type"),
            Some(&"application/octet-stream".to_string())
        );
    }
}
