use fold_rs::fabric::{symbol::Symbol, value::Value};
use fold_rs::thimble::apply_prim;

fn sym(name: &str) -> Symbol {
    Symbol::intern(name)
}

fn prim(op: &str, args: &[Value]) -> Value {
    apply_prim(&sym(op), args).expect("primitive failed")
}

fn expect_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        other => panic!("expected string, got {:?}", other),
    }
}

fn expect_char(value: &Value) -> char {
    match value {
        Value::Char(ch) => *ch,
        other => panic!("expected char, got {:?}", other),
    }
}

fn expect_bool(value: &Value) -> bool {
    match value {
        Value::Bool(b) => *b,
        other => panic!("expected bool, got {:?}", other),
    }
}

fn is_eof(value: &Value) -> bool {
    matches!(value, Value::Symbol(sym) if sym.as_str() == "#<eof>")
}

#[test]
fn test_open_input_string() {
    let port = prim("open-input-string", &[Value::String("hello".to_string())]);
    assert!(matches!(port, Value::Port(_)));

    // Test that it's recognized as a port
    let is_port = prim("port?", &[port.clone()]);
    assert!(expect_bool(&is_port));

    // Test that it's recognized as an input port
    let is_input = prim("input-port?", &[port.clone()]);
    assert!(expect_bool(&is_input));

    // Test that it's NOT an output port
    let is_output = prim("output-port?", &[port]);
    assert!(!expect_bool(&is_output));
}

#[test]
fn test_open_output_string() {
    let port = prim("open-output-string", &[]);
    assert!(matches!(port, Value::Port(_)));

    // Test that it's recognized as a port
    let is_port = prim("port?", &[port.clone()]);
    assert!(expect_bool(&is_port));

    // Test that it's recognized as an output port
    let is_output = prim("output-port?", &[port.clone()]);
    assert!(expect_bool(&is_output));

    // Test that it's NOT an input port
    let is_input = prim("input-port?", &[port]);
    assert!(!expect_bool(&is_input));
}

#[test]
fn test_read_char() {
    let port = prim("open-input-string", &[Value::String("abc".to_string())]);

    // Read 'a'
    let ch1 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch1), 'a');

    // Read 'b'
    let ch2 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch2), 'b');

    // Read 'c'
    let ch3 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch3), 'c');

    // Read EOF
    let eof = prim("read-char", &[port]);
    assert!(is_eof(&eof));
}

#[test]
fn test_peek_char() {
    let port = prim("open-input-string", &[Value::String("ab".to_string())]);

    // Peek 'a' (should not consume)
    let ch1 = prim("peek-char", &[port.clone()]);
    assert_eq!(expect_char(&ch1), 'a');

    // Peek again - should still be 'a'
    let ch2 = prim("peek-char", &[port.clone()]);
    assert_eq!(expect_char(&ch2), 'a');

    // Read 'a' (consumes it)
    let ch3 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch3), 'a');

    // Peek 'b'
    let ch4 = prim("peek-char", &[port.clone()]);
    assert_eq!(expect_char(&ch4), 'b');

    // Read 'b'
    let ch5 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch5), 'b');

    // Peek EOF
    let eof = prim("peek-char", &[port]);
    assert!(is_eof(&eof));
}

#[test]
fn test_write_char() {
    let port = prim("open-output-string", &[]);

    // Write 'h'
    prim("write-char", &[Value::Char('h'), port.clone()]);

    // Write 'i'
    prim("write-char", &[Value::Char('i'), port.clone()]);

    // Get output
    let output = prim("get-output-string", &[port]);
    assert_eq!(expect_string(&output), "hi");
}

#[test]
fn test_get_output_string() {
    let port = prim("open-output-string", &[]);

    // Initially empty
    let output1 = prim("get-output-string", &[port.clone()]);
    assert_eq!(expect_string(&output1), "");

    // Write some characters
    prim("write-char", &[Value::Char('a'), port.clone()]);
    prim("write-char", &[Value::Char('b'), port.clone()]);
    prim("write-char", &[Value::Char('c'), port.clone()]);

    // Get output
    let output2 = prim("get-output-string", &[port.clone()]);
    assert_eq!(expect_string(&output2), "abc");

    // Buffer should still contain the data (non-destructive read)
    let output3 = prim("get-output-string", &[port]);
    assert_eq!(expect_string(&output3), "abc");
}

#[test]
fn test_eof_object() {
    // Create EOF object
    let eof = prim("eof-object", &[]);

    // Test eof-object? predicate
    let is_eof = prim("eof-object?", &[eof.clone()]);
    assert!(expect_bool(&is_eof));

    // Non-EOF values should return false
    let not_eof1 = prim("eof-object?", &[Value::Char('a')]);
    assert!(!expect_bool(&not_eof1));

    let not_eof2 = prim("eof-object?", &[Value::String("eof".to_string())]);
    assert!(!expect_bool(&not_eof2));
}

#[test]
fn test_unicode_support() {
    // Test UTF-8 characters
    let port = prim("open-input-string", &[Value::String("你好".to_string())]);

    // Read first character '你'
    let ch1 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch1), '你');

    // Read second character '好'
    let ch2 = prim("read-char", &[port.clone()]);
    assert_eq!(expect_char(&ch2), '好');

    // EOF
    let eof = prim("read-char", &[port]);
    assert!(is_eof(&eof));
}

#[test]
fn test_output_unicode() {
    let port = prim("open-output-string", &[]);

    // Write Unicode characters
    prim("write-char", &[Value::Char('🎉'), port.clone()]);
    prim("write-char", &[Value::Char('世'), port.clone()]);
    prim("write-char", &[Value::Char('界'), port.clone()]);

    // Get output
    let output = prim("get-output-string", &[port]);
    assert_eq!(expect_string(&output), "🎉世界");
}
