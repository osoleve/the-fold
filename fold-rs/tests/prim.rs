use std::sync::{Mutex, OnceLock};

use fold_rs::fabric::{ADDRESS_SIZE, Address, symbol::Symbol, value::Value};
use fold_rs::thimble::apply_prim;

fn store_test_guard() -> std::sync::MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .expect("store lock")
}

fn sym(name: &str) -> Symbol {
    name.to_string()
}

fn prim(op: &str, args: &[Value]) -> Value {
    apply_prim(&sym(op), args).expect("primitive failed")
}

fn list_to_vec(list: &Value) -> Vec<Value> {
    match prim("list->vec", std::slice::from_ref(list)) {
        Value::Vector(values) => values,
        other => panic!("expected vector, got {:?}", other),
    }
}

fn expect_number(value: &Value) -> i64 {
    match value {
        Value::Number(n) => *n,
        other => panic!("expected number, got {:?}", other),
    }
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

fn expect_float(value: &Value) -> f64 {
    match value {
        Value::Float(n) => *n,
        other => panic!("expected float, got {:?}", other),
    }
}

fn expect_address(value: &Value) -> Address {
    match value {
        Value::Address(addr) => *addr,
        other => panic!("expected address, got {:?}", other),
    }
}

#[test]
fn string_ops() {
    let out = prim(
        "string-append",
        &[
            Value::String("hello".to_string()),
            Value::String(" ".to_string()),
            Value::String("world".to_string()),
        ],
    );
    assert_eq!(expect_string(&out), "hello world");

    let len = prim("string-length", &[Value::String("abc".to_string())]);
    assert_eq!(expect_number(&len), 3);

    let ch = prim(
        "string-ref",
        &[Value::String("abc".to_string()), Value::Number(1)],
    );
    assert_eq!(expect_char(&ch), 'b');

    let sub = prim(
        "substring",
        &[
            Value::String("hello".to_string()),
            Value::Number(1),
            Value::Number(4),
        ],
    );
    assert_eq!(expect_string(&sub), "ell");
}

#[test]
fn list_ops() {
    let list = prim(
        "list",
        &[Value::Number(1), Value::Number(2), Value::Number(3)],
    );
    let len = prim("length", std::slice::from_ref(&list));
    assert_eq!(expect_number(&len), 3);

    let reversed = prim("reverse", std::slice::from_ref(&list));
    let values = list_to_vec(&reversed);
    assert!(matches!(values[0], Value::Number(3)));
    assert!(matches!(values[1], Value::Number(2)));
    assert!(matches!(values[2], Value::Number(1)));

    let appended = prim("append", &[list.clone(), prim("list", &[Value::Number(4)])]);
    let values = list_to_vec(&appended);
    assert_eq!(values.len(), 4);
    assert!(matches!(values[3], Value::Number(4)));

    let second = prim("list-ref", &[list.clone(), Value::Number(1)]);
    assert!(matches!(second, Value::Number(2)));

    let dotted = Value::Pair(Box::new(Value::Number(1)), Box::new(Value::Number(2)));
    let is_list = prim("list?", &[dotted]);
    assert!(matches!(is_list, Value::Bool(false)));
}

#[test]
fn vector_ops() {
    let vec = prim("vec-make", &[Value::Number(1), Value::Number(2)]);
    let len = prim("vec-length", std::slice::from_ref(&vec));
    assert_eq!(expect_number(&len), 2);

    let second = prim("vec-ref", &[vec.clone(), Value::Number(1)]);
    assert!(matches!(second, Value::Number(2)));

    let list = prim("vec->list", std::slice::from_ref(&vec));
    let values = list_to_vec(&list);
    assert_eq!(values.len(), 2);

    let back = prim("list->vec", &[list]);
    match back {
        Value::Vector(values) => assert_eq!(values.len(), 2),
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn bytevector_ops() {
    let bv = prim("bv-make", &[Value::Number(3), Value::Number(7)]);
    match &bv {
        Value::Bytevector(bytes) => assert_eq!(bytes, &vec![7u8; 3]),
        other => panic!("expected bytevector, got {:?}", other),
    }

    let len = prim("bv-length", std::slice::from_ref(&bv));
    assert_eq!(expect_number(&len), 3);

    let ref_val = prim("bv-ref", &[bv.clone(), Value::Number(1)]);
    assert_eq!(expect_number(&ref_val), 7);

    let concat = prim(
        "bv-concat",
        &[Value::Bytevector(vec![1, 2]), Value::Bytevector(vec![3])],
    );
    match concat {
        Value::Bytevector(bytes) => assert_eq!(bytes, vec![1, 2, 3]),
        other => panic!("expected bytevector, got {:?}", other),
    }

    let slice = prim(
        "bv-slice",
        &[
            Value::Bytevector(vec![1, 2, 3, 4]),
            Value::Number(1),
            Value::Number(3),
        ],
    );
    match slice {
        Value::Bytevector(bytes) => assert_eq!(bytes, vec![2, 3]),
        other => panic!("expected bytevector, got {:?}", other),
    }
}

#[test]
fn conversions_and_preds() {
    let utf8 = prim("string->utf8", &[Value::String("hi".to_string())]);
    let round = prim("utf8->string", &[utf8]);
    assert_eq!(expect_string(&round), "hi");

    let sym = prim("string->symbol", &[Value::String("foo".to_string())]);
    match sym {
        Value::Symbol(name) => assert_eq!(name, "foo"),
        other => panic!("expected symbol, got {:?}", other),
    }

    let s = prim("symbol->string", &[Value::Symbol("bar".to_string())]);
    assert_eq!(expect_string(&s), "bar");

    let n = prim("number->string", &[Value::Number(42)]);
    assert_eq!(expect_string(&n), "42");

    let n = prim("string->number", &[Value::String("17".to_string())]);
    assert_eq!(expect_number(&n), 17);

    let bad = prim("string->number", &[Value::String("nope".to_string())]);
    assert!(matches!(bad, Value::Bool(false)));

    let pred = prim("number?", &[Value::Number(1)]);
    assert!(matches!(pred, Value::Bool(true)));

    let pred = prim("boolean?", &[Value::Bool(false)]);
    assert!(matches!(pred, Value::Bool(true)));
}

#[test]
fn float_ops() {
    let sum = prim("add", &[Value::Number(1), Value::Float(2.5)]);
    assert!((expect_float(&sum) - 3.5).abs() < 1e-9);

    let div = prim("div", &[Value::Float(7.5), Value::Number(2)]);
    assert!((expect_float(&div) - 3.75).abs() < 1e-9);

    let parsed = prim("string->number", &[Value::String("3.14".to_string())]);
    assert!((expect_float(&parsed) - std::f64::consts::PI).abs() < 1e-2);

    let is_number = prim("number?", &[Value::Float(1.0)]);
    assert!(matches!(is_number, Value::Bool(true)));

    let is_integer = prim("integer?", &[Value::Float(1.0)]);
    assert!(matches!(is_integer, Value::Bool(false)));
}

#[test]
fn memq_and_assq_ops() {
    let list = prim(
        "list",
        &[Value::Number(1), Value::Number(2), Value::Number(3)],
    );
    let found = prim("memq", &[Value::Number(2), list.clone()]);
    let values = list_to_vec(&found);
    assert_eq!(values.len(), 2);
    assert!(matches!(values[0], Value::Number(2)));
    assert!(matches!(values[1], Value::Number(3)));

    let missing = prim("memq", &[Value::Number(9), list]);
    assert!(matches!(missing, Value::Bool(false)));

    let pair_a = Value::Pair(
        Box::new(Value::Symbol("a".to_string())),
        Box::new(Value::Number(1)),
    );
    let pair_b = Value::Pair(
        Box::new(Value::Symbol("b".to_string())),
        Box::new(Value::Number(2)),
    );
    let pair_c = Value::Pair(
        Box::new(Value::Symbol("c".to_string())),
        Box::new(Value::Number(3)),
    );
    let alist = Value::Pair(
        Box::new(pair_a),
        Box::new(Value::Pair(
            Box::new(pair_b.clone()),
            Box::new(Value::Pair(Box::new(pair_c), Box::new(Value::Nil))),
        )),
    );

    let found = prim("assq", &[Value::Symbol("b".to_string()), alist]);
    match found {
        Value::Pair(car, cdr) => {
            assert!(matches!(*car, Value::Symbol(ref s) if s == "b"));
            assert!(matches!(*cdr, Value::Number(2)));
        }
        other => panic!("expected pair, got {:?}", other),
    }
}

#[test]
fn char_ops() {
    let code = prim("char->integer", &[Value::Char('A')]);
    assert_eq!(expect_number(&code), 65);

    let ch = prim("integer->char", &[Value::Number(97)]);
    assert_eq!(expect_char(&ch), 'a');

    let up = prim("char-upcase", &[Value::Char('a')]);
    assert_eq!(expect_char(&up), 'A');

    let down = prim("char-downcase", &[Value::Char('A')]);
    assert_eq!(expect_char(&down), 'a');
}

#[test]
fn cas_prims() {
    let _guard = store_test_guard();

    let payload = prim("string->utf8", &[Value::String("hello".to_string())]);
    let refs = prim("vec-empty", &[]);
    let block = prim(
        "make-block",
        &[Value::Symbol("test".to_string()), payload.clone(), refs],
    );

    let digest = prim("sha256", &[payload]);
    match digest {
        Value::Bytevector(bytes) => assert_eq!(bytes.len(), 32),
        other => panic!("expected bytevector, got {:?}", other),
    }

    let addr = expect_address(&prim("hash-block", std::slice::from_ref(&block)));
    let hex = prim("hash->hex", &[Value::Address(addr)]);
    let addr_roundtrip = expect_address(&prim("hex->hash", &[hex]));
    assert_eq!(addr, addr_roundtrip);

    let stored = expect_address(&prim("store!", std::slice::from_ref(&block)));
    let fetched = prim("fetch", &[Value::Address(stored)]);
    match fetched {
        Value::Block(stored_block) => assert_eq!(stored_block.tag, "test"),
        other => panic!("expected block, got {:?}", other),
    }

    let missing = prim(
        "fetch",
        &[Value::Address(Address::from([0u8; ADDRESS_SIZE]))],
    );
    assert!(matches!(missing, Value::Bool(false)));
}

#[test]
fn cas_store_helpers() {
    let _guard = store_test_guard();

    let child_payload = prim(
        "string->utf8",
        &[Value::String("cas-helper-child".to_string())],
    );
    let child_block = prim(
        "make-block",
        &[
            Value::Symbol("child".to_string()),
            child_payload,
            prim("vec-empty", &[]),
        ],
    );
    let child_addr = expect_address(&prim("store!", &[child_block]));

    let parent_payload = prim(
        "string->utf8",
        &[Value::String("cas-helper-parent".to_string())],
    );
    let parent_refs = prim("vec-make", &[Value::Address(child_addr)]);
    let parent_block = prim(
        "make-block",
        &[
            Value::Symbol("parent".to_string()),
            parent_payload,
            parent_refs,
        ],
    );
    let parent_addr = expect_address(&prim("store!", &[parent_block]));

    let stored = prim("stored?", &[Value::Address(child_addr)]);
    assert!(matches!(stored, Value::Bool(true)));

    let pinned = prim("pinned?", &[Value::Address(child_addr)]);
    assert!(matches!(pinned, Value::Bool(false)));
    let pin = prim("pin!", &[Value::Address(child_addr)]);
    assert!(matches!(pin, Value::Nil));
    let pinned = prim("pinned?", &[Value::Address(child_addr)]);
    assert!(matches!(pinned, Value::Bool(true)));
    let unpin = prim("unpin!", &[Value::Address(child_addr)]);
    assert!(matches!(unpin, Value::Nil));
    let pinned = prim("pinned?", &[Value::Address(child_addr)]);
    assert!(matches!(pinned, Value::Bool(false)));

    let unpin_parent = prim("unpin!", &[Value::Address(parent_addr)]);
    assert!(matches!(unpin_parent, Value::Nil));
    let count = prim("pin-tree!", &[Value::Address(parent_addr)]);
    assert_eq!(expect_number(&count), 2);
    let count = prim("unpin-tree!", &[Value::Address(parent_addr)]);
    assert_eq!(expect_number(&count), 2);

    let hashes = prim("store-hashes", &[]);
    let values = list_to_vec(&hashes);
    assert!(
        values
            .iter()
            .any(|v| matches!(v, Value::Address(a) if *a == child_addr))
    );
    assert!(
        values
            .iter()
            .any(|v| matches!(v, Value::Address(a) if *a == parent_addr))
    );
}

#[test]
fn block_bytes_roundtrip() {
    let payload = prim("string->utf8", &[Value::String("payload".to_string())]);
    let refs = prim("vec-empty", &[]);
    let block = prim(
        "make-block",
        &[Value::Symbol("node".to_string()), payload, refs],
    );

    let bytes = prim("block->bytes", std::slice::from_ref(&block));
    let restored = prim("bytes->block", &[bytes]);
    match restored {
        Value::Block(block) => {
            assert_eq!(block.tag, "node");
            assert_eq!(block.payload, b"payload");
        }
        other => panic!("expected block, got {:?}", other),
    }
}

#[test]
fn math_utils() {
    // exp
    let e = prim("exp", &[Value::Number(1)]);
    let e_val = expect_float(&e);
    assert!((e_val - std::f64::consts::E).abs() < 1e-10);

    // exp(0) = 1
    let exp0 = prim("exp", &[Value::Number(0)]);
    assert!((expect_float(&exp0) - 1.0).abs() < 1e-10);
}

#[test]
fn interpolation_utils() {
    // lerp: (lerp 0 10 0.5) = 5
    let lerp_result = prim(
        "lerp",
        &[Value::Number(0), Value::Number(10), Value::Float(0.5)],
    );
    assert!((expect_float(&lerp_result) - 5.0).abs() < 1e-10);

    // lerp: (lerp 0 10 0) = 0
    let lerp_start = prim(
        "lerp",
        &[Value::Number(0), Value::Number(10), Value::Float(0.0)],
    );
    assert!((expect_float(&lerp_start) - 0.0).abs() < 1e-10);

    // lerp: (lerp 0 10 1) = 10
    let lerp_end = prim(
        "lerp",
        &[Value::Number(0), Value::Number(10), Value::Float(1.0)],
    );
    assert!((expect_float(&lerp_end) - 10.0).abs() < 1e-10);

    // inverse-lerp: (inverse-lerp 0 10 5) = 0.5
    let inv_lerp = prim(
        "inverse-lerp",
        &[Value::Number(0), Value::Number(10), Value::Number(5)],
    );
    assert!((expect_float(&inv_lerp) - 0.5).abs() < 1e-10);

    // scale: (scale 5 0 10 0 100) = 50
    let scale_result = prim(
        "scale",
        &[
            Value::Number(5),
            Value::Number(0),
            Value::Number(10),
            Value::Number(0),
            Value::Number(100),
        ],
    );
    assert!((expect_float(&scale_result) - 50.0).abs() < 1e-10);

    // smoothstep: (smoothstep 0 1 0.5) should be ~0.5 (but slightly different due to curve)
    let smooth = prim(
        "smoothstep",
        &[Value::Number(0), Value::Number(1), Value::Float(0.5)],
    );
    let smooth_val = expect_float(&smooth);
    assert!(smooth_val > 0.4 && smooth_val < 0.6);

    // denormalize: (denormalize 0.5 0 100) = 50
    let denorm = prim(
        "denormalize",
        &[Value::Float(0.5), Value::Number(0), Value::Number(100)],
    );
    assert!((expect_float(&denorm) - 50.0).abs() < 1e-10);

    // percent-of: (percent-of 25 100) = 25.0
    let pct = prim("percent-of", &[Value::Number(25), Value::Number(100)]);
    assert!((expect_float(&pct) - 25.0).abs() < 1e-10);

    // percent-change: (percent-change 100 150) = 50.0
    let pct_change = prim("percent-change", &[Value::Number(100), Value::Number(150)]);
    assert!((expect_float(&pct_change) - 50.0).abs() < 1e-10);

    // round-to: (round-to 12.34567 2) = 12.35
    let rounded = prim("round-to", &[Value::Float(12.34567), Value::Number(2)]);
    assert!((expect_float(&rounded) - 12.35).abs() < 1e-10);

    // saturation-add: (saturation-add 90 20 0 100) = 100 (clamped)
    let sat_add = prim(
        "saturation-add",
        &[
            Value::Number(90),
            Value::Number(20),
            Value::Number(0),
            Value::Number(100),
        ],
    );
    assert!((expect_float(&sat_add) - 100.0).abs() < 1e-10);
}

#[test]
fn assoc_ops() {
    // Create alist: ((a . 1) (b . 2) (c . 3))
    let pair_a = prim("cons", &[Value::Symbol("a".to_string()), Value::Number(1)]);
    let pair_b = prim("cons", &[Value::Symbol("b".to_string()), Value::Number(2)]);
    let pair_c = prim("cons", &[Value::Symbol("c".to_string()), Value::Number(3)]);
    let alist = prim("list", &[pair_a, pair_b, pair_c]);

    // assoc finds by key
    let found = prim("assoc", &[Value::Symbol("b".to_string()), alist.clone()]);
    match found {
        Value::Pair(car, cdr) => {
            assert!(matches!(*car, Value::Symbol(ref s) if s == "b"));
            assert!(matches!(*cdr, Value::Number(2)));
        }
        other => panic!("expected pair, got {:?}", other),
    }

    // assq is an alias
    let found2 = prim("assq", &[Value::Symbol("a".to_string()), alist.clone()]);
    match found2 {
        Value::Pair(car, cdr) => {
            assert!(matches!(*car, Value::Symbol(ref s) if s == "a"));
            assert!(matches!(*cdr, Value::Number(1)));
        }
        other => panic!("expected pair, got {:?}", other),
    }

    // Not found returns #f
    let not_found = prim("assoc", &[Value::Symbol("z".to_string()), alist]);
    assert!(matches!(not_found, Value::Bool(false)));
}

#[test]
fn string_utilities() {
    // string-words splits on whitespace
    let words = prim(
        "string-words",
        &[Value::String("hello world foo".to_string())],
    );
    let vec = list_to_vec(&words);
    assert_eq!(vec.len(), 3);
    assert_eq!(expect_string(&vec[0]), "hello");
    assert_eq!(expect_string(&vec[1]), "world");
    assert_eq!(expect_string(&vec[2]), "foo");

    // string-lines splits on newlines
    let lines = prim(
        "string-lines",
        &[Value::String("line1\nline2\nline3".to_string())],
    );
    let vec = list_to_vec(&lines);
    assert_eq!(vec.len(), 3);
    assert_eq!(expect_string(&vec[0]), "line1");
    assert_eq!(expect_string(&vec[1]), "line2");
    assert_eq!(expect_string(&vec[2]), "line3");

    // string-join concatenates with separator
    let list = prim(
        "list",
        &[
            Value::String("a".to_string()),
            Value::String("b".to_string()),
            Value::String("c".to_string()),
        ],
    );
    let joined = prim("string-join", &[list, Value::String(", ".to_string())]);
    assert_eq!(expect_string(&joined), "a, b, c");

    // string-replace-all replaces all occurrences
    let replaced = prim(
        "string-replace-all",
        &[
            Value::String("foo bar foo baz foo".to_string()),
            Value::String("foo".to_string()),
            Value::String("X".to_string()),
        ],
    );
    assert_eq!(expect_string(&replaced), "X bar X baz X");
}

#[test]
fn string_trim_operations() {
    // string-trim-start removes leading whitespace
    let trimmed = prim(
        "string-trim-start",
        &[Value::String("  hello  ".to_string())],
    );
    assert_eq!(expect_string(&trimmed), "hello  ");

    // string-trim-end removes trailing whitespace
    let trimmed = prim("string-trim-end", &[Value::String("  hello  ".to_string())]);
    assert_eq!(expect_string(&trimmed), "  hello");

    // string-trim-both removes both
    let trimmed = prim(
        "string-trim-both",
        &[Value::String("  hello  ".to_string())],
    );
    assert_eq!(expect_string(&trimmed), "hello");
}

#[test]
fn string_predicates() {
    // string-ascii?
    assert!(matches!(
        prim("string-ascii?", &[Value::String("hello123".to_string())]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("string-ascii?", &[Value::String("héllo".to_string())]),
        Value::Bool(false)
    ));

    // string-numeric?
    assert!(matches!(
        prim("string-numeric?", &[Value::String("12345".to_string())]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("string-numeric?", &[Value::String("123a5".to_string())]),
        Value::Bool(false)
    ));

    // string-alphabetic?
    assert!(matches!(
        prim("string-alphabetic?", &[Value::String("hello".to_string())]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim(
            "string-alphabetic?",
            &[Value::String("hello123".to_string())]
        ),
        Value::Bool(false)
    ));

    // string-alphanumeric?
    assert!(matches!(
        prim(
            "string-alphanumeric?",
            &[Value::String("hello123".to_string())]
        ),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim(
            "string-alphanumeric?",
            &[Value::String("hello 123".to_string())]
        ),
        Value::Bool(false)
    ));

    // string-whitespace?
    assert!(matches!(
        prim(
            "string-whitespace?",
            &[Value::String("   \t\n".to_string())]
        ),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("string-whitespace?", &[Value::String("  x  ".to_string())]),
        Value::Bool(false)
    ));

    // string-empty?
    assert!(matches!(
        prim("string-empty?", &[Value::String("".to_string())]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("string-empty?", &[Value::String("x".to_string())]),
        Value::Bool(false)
    ));
}

#[test]
fn vector_operations() {
    // vec-copy
    let vec = Value::Vector(vec![Value::Number(1), Value::Number(2), Value::Number(3)]);
    let copied = prim("vec-copy", std::slice::from_ref(&vec));
    match copied {
        Value::Vector(v) => {
            assert_eq!(v.len(), 3);
            assert_eq!(expect_number(&v[0]), 1);
        }
        other => panic!("expected vector, got {:?}", other),
    }

    // vec-slice
    let sliced = prim(
        "vec-slice",
        &[vec.clone(), Value::Number(1), Value::Number(3)],
    );
    match sliced {
        Value::Vector(v) => {
            assert_eq!(v.len(), 2);
            assert_eq!(expect_number(&v[0]), 2);
            assert_eq!(expect_number(&v[1]), 3);
        }
        other => panic!("expected vector, got {:?}", other),
    }

    // vec-reverse
    let reversed = prim("vec-reverse", std::slice::from_ref(&vec));
    match reversed {
        Value::Vector(v) => {
            assert_eq!(expect_number(&v[0]), 3);
            assert_eq!(expect_number(&v[1]), 2);
            assert_eq!(expect_number(&v[2]), 1);
        }
        other => panic!("expected vector, got {:?}", other),
    }

    // vec-contains?
    assert!(matches!(
        prim("vec-contains?", &[vec.clone(), Value::Number(2)]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("vec-contains?", &[vec.clone(), Value::Number(99)]),
        Value::Bool(false)
    ));

    // vec-index-of
    assert_eq!(
        expect_number(&prim("vec-index-of", &[vec.clone(), Value::Number(2)])),
        1
    );
    assert_eq!(
        expect_number(&prim("vec-index-of", &[vec.clone(), Value::Number(99)])),
        -1
    );
}

#[test]
fn type_introspection() {
    // type-of returns a symbol
    fn expect_symbol(value: &Value) -> String {
        match value {
            Value::Symbol(s) => s.clone(),
            other => panic!("expected symbol, got {:?}", other),
        }
    }

    assert_eq!(
        expect_symbol(&prim("type-of", &[Value::Number(42)])),
        "number"
    );
    assert_eq!(
        expect_symbol(&prim("type-of", &[Value::String("hi".to_string())])),
        "string"
    );
    assert_eq!(
        expect_symbol(&prim("type-of", &[Value::Vector(vec![Value::Number(1)])])),
        "vector"
    );
    assert_eq!(
        expect_symbol(&prim("type-of", &[Value::Symbol("foo".to_string())])),
        "symbol"
    );
    assert_eq!(
        expect_symbol(&prim("type-of", &[Value::Bool(true)])),
        "bool"
    );
    assert_eq!(expect_symbol(&prim("type-of", &[Value::Nil])), "nil");

    // is-number?, is-string?, is-vector?, is-list?
    assert!(matches!(
        prim("is-number?", &[Value::Number(42)]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("is-number?", &[Value::String("x".to_string())]),
        Value::Bool(false)
    ));

    assert!(matches!(
        prim("is-string?", &[Value::String("x".to_string())]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("is-vector?", &[Value::Vector(vec![])]),
        Value::Bool(true)
    ));
}

#[test]
fn comparison_utilities() {
    // equals? does deep equality
    assert!(matches!(
        prim("equals?", &[Value::Number(42), Value::Number(42)]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("equals?", &[Value::Number(42), Value::Number(43)]),
        Value::Bool(false)
    ));

    // not-equals?
    assert!(matches!(
        prim("not-equals?", &[Value::Number(42), Value::Number(43)]),
        Value::Bool(true)
    ));
    assert!(matches!(
        prim("not-equals?", &[Value::Number(42), Value::Number(42)]),
        Value::Bool(false)
    ));

    // hash-value produces consistent hashes
    let h1 = prim("hash-value", &[Value::String("test".to_string())]);
    let h2 = prim("hash-value", &[Value::String("test".to_string())]);
    assert_eq!(expect_number(&h1), expect_number(&h2));

    let h3 = prim("hash-value", &[Value::String("different".to_string())]);
    assert_ne!(expect_number(&h1), expect_number(&h3));
}
