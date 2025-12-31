use fold_rs::fabric::{symbol::Symbol, value::Value};
use fold_rs::thimble::apply_prim;

fn sym(name: &str) -> Symbol {
    Symbol::intern(name)
}

fn prim(op: &str, args: &[Value]) -> Value {
    apply_prim(&sym(op), args).expect("primitive failed")
}

fn expect_number(value: &Value) -> i64 {
    match value {
        Value::Number(n) => *n,
        other => panic!("expected number, got {:?}", other),
    }
}

fn expect_bool(value: &Value) -> bool {
    match value {
        Value::Bool(b) => *b,
        other => panic!("expected bool, got {:?}", other),
    }
}

fn list_to_vec(list: &Value) -> Vec<Value> {
    let mut result = Vec::new();
    let mut current = list;
    loop {
        match current {
            Value::Nil => break,
            Value::Pair(car, cdr) => {
                result.push(car.as_ref().clone());
                current = cdr;
            }
            _ => panic!("not a proper list"),
        }
    }
    result
}

#[test]
fn test_make_hashtable() {
    let ht = prim("make-hashtable", &[]);
    assert!(matches!(ht, Value::Hashtable(_)));
}

#[test]
fn test_hashtable_predicate() {
    let ht = prim("make-hashtable", &[]);
    let result = prim("hashtable?", &[ht]);
    assert!(expect_bool(&result));

    let not_ht = Value::Number(42);
    let result = prim("hashtable?", &[not_ht]);
    assert!(!expect_bool(&result));
}

#[test]
fn test_hashtable_set_and_ref() {
    let ht = prim("make-hashtable", &[]);
    let key = Value::Symbol(sym("foo"));
    let value = Value::Number(42);
    let default = Value::Number(-1);

    // Set a value
    prim("hashtable-set!", &[ht.clone(), key.clone(), value.clone()]);

    // Retrieve the value
    let result = prim("hashtable-ref", &[ht.clone(), key.clone(), default.clone()]);
    assert_eq!(expect_number(&result), 42);

    // Try to get a non-existent key
    let missing_key = Value::Symbol(sym("missing"));
    let result = prim("hashtable-ref", &[ht.clone(), missing_key, default.clone()]);
    assert_eq!(expect_number(&result), -1);
}

#[test]
fn test_hashtable_contains() {
    let ht = prim("make-hashtable", &[]);
    let key = Value::Symbol(sym("test"));
    let value = Value::String("hello".to_string());

    // Before insertion
    let result = prim("hashtable-contains?", &[ht.clone(), key.clone()]);
    assert!(!expect_bool(&result));

    // After insertion
    prim("hashtable-set!", &[ht.clone(), key.clone(), value]);
    let result = prim("hashtable-contains?", &[ht.clone(), key.clone()]);
    assert!(expect_bool(&result));
}

#[test]
fn test_hashtable_delete() {
    let ht = prim("make-hashtable", &[]);
    let key = Value::Number(1);
    let value = Value::String("one".to_string());

    // Insert and verify
    prim("hashtable-set!", &[ht.clone(), key.clone(), value]);
    let result = prim("hashtable-contains?", &[ht.clone(), key.clone()]);
    assert!(expect_bool(&result));

    // Delete and verify
    prim("hashtable-delete!", &[ht.clone(), key.clone()]);
    let result = prim("hashtable-contains?", &[ht.clone(), key.clone()]);
    assert!(!expect_bool(&result));
}

#[test]
fn test_hashtable_size() {
    let ht = prim("make-hashtable", &[]);

    // Empty hashtable
    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 0);

    // Add some entries
    prim("hashtable-set!", &[ht.clone(), Value::Number(1), Value::String("one".to_string())]);
    prim("hashtable-set!", &[ht.clone(), Value::Number(2), Value::String("two".to_string())]);
    prim("hashtable-set!", &[ht.clone(), Value::Number(3), Value::String("three".to_string())]);

    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 3);

    // Delete one
    prim("hashtable-delete!", &[ht.clone(), Value::Number(2)]);
    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 2);
}

#[test]
fn test_hashtable_keys() {
    let ht = prim("make-hashtable", &[]);

    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("a")), Value::Number(1)]);
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("b")), Value::Number(2)]);
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("c")), Value::Number(3)]);

    let keys = prim("hashtable-keys", &[ht.clone()]);
    let keys_vec = list_to_vec(&keys);

    assert_eq!(keys_vec.len(), 3);
    // Check that all keys are present (order may vary)
    assert!(keys_vec.contains(&Value::Symbol(sym("a"))));
    assert!(keys_vec.contains(&Value::Symbol(sym("b"))));
    assert!(keys_vec.contains(&Value::Symbol(sym("c"))));
}

#[test]
fn test_hashtable_values() {
    let ht = prim("make-hashtable", &[]);

    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("x")), Value::Number(10)]);
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("y")), Value::Number(20)]);
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("z")), Value::Number(30)]);

    let values = prim("hashtable-values", &[ht.clone()]);
    let values_vec = list_to_vec(&values);

    assert_eq!(values_vec.len(), 3);
    // Check that all values are present (order may vary)
    assert!(values_vec.contains(&Value::Number(10)));
    assert!(values_vec.contains(&Value::Number(20)));
    assert!(values_vec.contains(&Value::Number(30)));
}

#[test]
fn test_hashtable_to_alist() {
    let ht = prim("make-hashtable", &[]);

    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("name")), Value::String("Alice".to_string())]);
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("age")), Value::Number(30)]);

    let alist = prim("hashtable->alist", &[ht.clone()]);
    let pairs = list_to_vec(&alist);

    assert_eq!(pairs.len(), 2);

    // Check that all pairs are present
    for pair in &pairs {
        match pair {
            Value::Pair(key, value) => {
                match key.as_ref() {
                    Value::Symbol(s) if s.as_str() == "name" => {
                        match value.as_ref() {
                            Value::String(s) => assert_eq!(s, "Alice"),
                            _ => panic!("expected string value for name"),
                        }
                    }
                    Value::Symbol(s) if s.as_str() == "age" => {
                        assert_eq!(expect_number(value), 30);
                    }
                    _ => panic!("unexpected key in alist"),
                }
            }
            _ => panic!("expected pair in alist"),
        }
    }
}

#[test]
fn test_alist_to_hashtable() {
    // Create an alist: ((a . 1) (b . 2) (c . 3))
    let pair1 = Value::Pair(
        Box::new(Value::Symbol(sym("a"))),
        Box::new(Value::Number(1)),
    );
    let pair2 = Value::Pair(
        Box::new(Value::Symbol(sym("b"))),
        Box::new(Value::Number(2)),
    );
    let pair3 = Value::Pair(
        Box::new(Value::Symbol(sym("c"))),
        Box::new(Value::Number(3)),
    );

    let alist = Value::Pair(
        Box::new(pair1),
        Box::new(Value::Pair(
            Box::new(pair2),
            Box::new(Value::Pair(Box::new(pair3), Box::new(Value::Nil))),
        )),
    );

    let ht = prim("alist->hashtable", &[alist]);

    // Verify the hashtable contains the expected values
    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 3);

    let val_a = prim(
        "hashtable-ref",
        &[ht.clone(), Value::Symbol(sym("a")), Value::Number(-1)],
    );
    assert_eq!(expect_number(&val_a), 1);

    let val_b = prim(
        "hashtable-ref",
        &[ht.clone(), Value::Symbol(sym("b")), Value::Number(-1)],
    );
    assert_eq!(expect_number(&val_b), 2);

    let val_c = prim(
        "hashtable-ref",
        &[ht.clone(), Value::Symbol(sym("c")), Value::Number(-1)],
    );
    assert_eq!(expect_number(&val_c), 3);
}

#[test]
fn test_hashtable_different_key_types() {
    let ht = prim("make-hashtable", &[]);

    // Symbol key
    prim("hashtable-set!", &[ht.clone(), Value::Symbol(sym("key1")), Value::Number(1)]);

    // String key
    prim("hashtable-set!", &[ht.clone(), Value::String("key2".to_string()), Value::Number(2)]);

    // Number key
    prim("hashtable-set!", &[ht.clone(), Value::Number(42), Value::Number(3)]);

    // Char key
    prim("hashtable-set!", &[ht.clone(), Value::Char('x'), Value::Number(4)]);

    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 4);

    // Verify retrieval
    let val1 = prim("hashtable-ref", &[ht.clone(), Value::Symbol(sym("key1")), Value::Number(-1)]);
    assert_eq!(expect_number(&val1), 1);

    let val2 = prim("hashtable-ref", &[ht.clone(), Value::String("key2".to_string()), Value::Number(-1)]);
    assert_eq!(expect_number(&val2), 2);

    let val3 = prim("hashtable-ref", &[ht.clone(), Value::Number(42), Value::Number(-1)]);
    assert_eq!(expect_number(&val3), 3);

    let val4 = prim("hashtable-ref", &[ht.clone(), Value::Char('x'), Value::Number(-1)]);
    assert_eq!(expect_number(&val4), 4);
}

#[test]
fn test_hashtable_roundtrip() {
    let ht1 = prim("make-hashtable", &[]);

    prim("hashtable-set!", &[ht1.clone(), Value::Symbol(sym("x")), Value::Number(10)]);
    prim("hashtable-set!", &[ht1.clone(), Value::Symbol(sym("y")), Value::Number(20)]);
    prim("hashtable-set!", &[ht1.clone(), Value::Symbol(sym("z")), Value::Number(30)]);

    // Convert to alist
    let alist = prim("hashtable->alist", &[ht1.clone()]);

    // Convert back to hashtable
    let ht2 = prim("alist->hashtable", &[alist]);

    // Verify the new hashtable has the same contents
    let size = prim("hashtable-size", &[ht2.clone()]);
    assert_eq!(expect_number(&size), 3);

    let val_x = prim("hashtable-ref", &[ht2.clone(), Value::Symbol(sym("x")), Value::Number(-1)]);
    assert_eq!(expect_number(&val_x), 10);

    let val_y = prim("hashtable-ref", &[ht2.clone(), Value::Symbol(sym("y")), Value::Number(-1)]);
    assert_eq!(expect_number(&val_y), 20);

    let val_z = prim("hashtable-ref", &[ht2.clone(), Value::Symbol(sym("z")), Value::Number(-1)]);
    assert_eq!(expect_number(&val_z), 30);
}

#[test]
fn test_hashtable_mutation() {
    let ht = prim("make-hashtable", &[]);
    let key = Value::Symbol(sym("counter"));

    // Set initial value
    prim("hashtable-set!", &[ht.clone(), key.clone(), Value::Number(0)]);

    // Mutate multiple times
    for i in 1..=5 {
        prim("hashtable-set!", &[ht.clone(), key.clone(), Value::Number(i)]);
        let val = prim("hashtable-ref", &[ht.clone(), key.clone(), Value::Number(-1)]);
        assert_eq!(expect_number(&val), i);
    }

    // Size should still be 1
    let size = prim("hashtable-size", &[ht.clone()]);
    assert_eq!(expect_number(&size), 1);
}
