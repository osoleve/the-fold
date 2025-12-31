use std::env;
use std::hint::black_box;
use std::time::Instant;

use fold_rs::fabric::{Block, Symbol, Value};
use fold_rs::thimble::apply_prim;

fn main() {
    let iterations = env::var("FOLD_BENCH_ITERS")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(100_000);

    println!();
    println!("=== Rust Primitive Benchmarks ===");
    println!("Iterations per test: {}", iterations);
    println!();

    let test_list = list_from_values(&(1..=10).map(Value::Number).collect::<Vec<_>>());
    let test_vec = Value::Vector((1..=10).map(Value::Number).collect::<Vec<_>>());
    let test_bv = Value::Bytevector(vec![42u8; 32]);
    let test_string = Value::String("hello world".to_string());
    let test_block = Value::Block(Block::new(
        Symbol::intern("test"),
        b"payload".to_vec(),
        Vec::new(),
    ));
    let alist = {
        let pair_a = Value::Pair(
            Box::new(Value::Symbol(Symbol::intern("a"))),
            Box::new(Value::Number(1)),
        );
        let pair_b = Value::Pair(
            Box::new(Value::Symbol(Symbol::intern("b"))),
            Box::new(Value::Number(2)),
        );
        let pair_c = Value::Pair(
            Box::new(Value::Symbol(Symbol::intern("c"))),
            Box::new(Value::Number(3)),
        );
        Value::Pair(
            Box::new(pair_a),
            Box::new(Value::Pair(
                Box::new(pair_b),
                Box::new(Value::Pair(Box::new(pair_c), Box::new(Value::Nil))),
            )),
        )
    };

    println!("--- Tier 1 (Cost 1) - O(1) trivial ---");
    bench_prim("number?", &[Value::Number(42)], iterations);
    bench_prim("symbol?", &[Value::Symbol(Symbol::intern("foo"))], iterations);
    bench_prim("null?", &[Value::Nil], iterations);
    bench_prim(
        "pair?",
        &[Value::Pair(
            Box::new(Value::Number(1)),
            Box::new(Value::Number(2)),
        )],
        iterations,
    );
    bench_prim(
        "car",
        &[Value::Pair(
            Box::new(Value::Number(1)),
            Box::new(Value::Number(2)),
        )],
        iterations,
    );
    bench_prim(
        "cdr",
        &[Value::Pair(
            Box::new(Value::Number(1)),
            Box::new(Value::Number(2)),
        )],
        iterations,
    );
    bench_prim("not", &[Value::Bool(false)], iterations);
    bench_prim("eq?", &[Value::Number(42), Value::Number(42)], iterations);
    bench_prim("zero?", &[Value::Number(0)], iterations);
    bench_prim("block-tag", std::slice::from_ref(&test_block), iterations);

    println!();
    println!("--- Tier 2 (Cost 2) - O(1) simple ---");
    bench_prim("add", &[Value::Number(100), Value::Number(200)], iterations);
    bench_prim("sub", &[Value::Number(100), Value::Number(50)], iterations);
    bench_prim("mul", &[Value::Number(12), Value::Number(34)], iterations);
    bench_prim("neg", &[Value::Number(42)], iterations);
    bench_prim("lt?", &[Value::Number(10), Value::Number(20)], iterations);
    bench_prim("cons", &[Value::Number(1), Value::Number(2)], iterations);
    bench_prim("char->integer", &[Value::Char('A')], iterations);
    bench_prim("vec-ref", &[test_vec.clone(), Value::Number(5)], iterations);
    bench_prim("bv-ref", &[test_bv.clone(), Value::Number(10)], iterations);
    bench_prim(
        "string-ref",
        &[test_string.clone(), Value::Number(5)],
        iterations,
    );
    bench_prim(
        "string-length",
        std::slice::from_ref(&test_string),
        iterations,
    );
    bench_prim("vec-length", std::slice::from_ref(&test_vec), iterations);

    println!();
    println!("--- Tier 3 (Cost 3) - O(1) moderate ---");
    bench_prim("div", &[Value::Number(100), Value::Number(7)], iterations);
    bench_prim("mod", &[Value::Number(100), Value::Number(7)], iterations);
    bench_prim(
        "bitand",
        &[Value::Number(0xFF00), Value::Number(0x0FF0)],
        iterations,
    );
    bench_prim(
        "bitor",
        &[Value::Number(0xFF00), Value::Number(0x00FF)],
        iterations,
    );
    bench_prim(
        "bitxor",
        &[Value::Number(0xAAAA), Value::Number(0x5555)],
        iterations,
    );
    bench_prim("shl", &[Value::Number(1), Value::Number(10)], iterations);
    bench_prim("shr", &[Value::Number(1024), Value::Number(5)], iterations);
    bench_prim(
        "string=?",
        &[
            Value::String("hello".to_string()),
            Value::String("hello".to_string()),
        ],
        iterations,
    );
    bench_prim(
        "string<?",
        &[
            Value::String("aaa".to_string()),
            Value::String("bbb".to_string()),
        ],
        iterations,
    );
    bench_prim("memq", &[Value::Number(5), test_list.clone()], iterations);
    bench_prim(
        "assq",
        &[Value::Symbol(Symbol::intern("b")), alist.clone()],
        iterations,
    );

    println!();
    println!("--- Tier 4 (Cost 5) - O(n) linear ---");
    bench_prim("length", std::slice::from_ref(&test_list), iterations);
    bench_prim("reverse", std::slice::from_ref(&test_list), iterations);
    bench_prim("vec->list", std::slice::from_ref(&test_vec), iterations);
    bench_prim("list->vec", std::slice::from_ref(&test_list), iterations);
    bench_prim(
        "string->list",
        std::slice::from_ref(&test_string),
        iterations,
    );
    bench_prim(
        "list->string",
        &[list_from_values(&[
            Value::Char('h'),
            Value::Char('e'),
            Value::Char('l'),
            Value::Char('l'),
            Value::Char('o'),
        ])],
        iterations,
    );
    bench_prim(
        "symbol->string",
        &[Value::Symbol(Symbol::intern("hello"))],
        iterations,
    );
    bench_prim(
        "string->symbol",
        &[Value::String("hello".to_string())],
        iterations,
    );

    println!();
    println!("--- Tier 5 (Cost 10) - O(n) with allocation ---");
    bench_prim(
        "string-append",
        &[
            Value::String("hello".to_string()),
            Value::String(" ".to_string()),
            Value::String("world".to_string()),
        ],
        iterations,
    );
    bench_prim(
        "substring",
        &[test_string.clone(), Value::Number(0), Value::Number(5)],
        iterations,
    );
    bench_prim(
        "bv-slice",
        &[test_bv.clone(), Value::Number(0), Value::Number(16)],
        iterations,
    );
    bench_prim(
        "string->utf8",
        std::slice::from_ref(&test_string),
        iterations,
    );
    bench_prim(
        "utf8->string",
        &[Value::Bytevector(b"hello world".to_vec())],
        iterations,
    );
    bench_prim(
        "block->bytes",
        std::slice::from_ref(&test_block),
        iterations,
    );
    let block_bytes_sym = Symbol::intern("block->bytes");
    let block_bytes = match apply_prim(
        &block_bytes_sym,
        std::slice::from_ref(&test_block),
    ) {
        Ok(Value::Bytevector(bytes)) => Value::Bytevector(bytes),
        Ok(_) => panic!("block->bytes did not return bytevector"),
        Err(err) => panic!("block->bytes failed: {err:?}"),
    };
    bench_prim("bytes->block", &[block_bytes], iterations);
    bench_prim(
        "string->number",
        &[Value::String("12345".to_string())],
        iterations,
    );

    println!();
    println!("--- Tier 6 (Cost 15) - Expensive conversions ---");
    bench_prim("number->string", &[Value::Number(12345)], iterations);

    println!();
    println!("--- Tier 7 (Cost 100) - Cryptographic ---");
    bench_prim("sha256", std::slice::from_ref(&test_bv), iterations);

    println!();
    println!("--- Tier 8 (Cost 110) - Serialization + crypto ---");
    bench_prim("hash-block", std::slice::from_ref(&test_block), iterations);
}

fn bench_prim(op: &str, args: &[Value], iterations: usize) {
    let op_sym = Symbol::intern(op);
    bench(op, iterations, || {
        let result = apply_prim(&op_sym, args).expect("primitive failed");
        black_box(result);
    });
}

fn bench<F>(name: &str, iterations: usize, mut f: F)
where
    F: FnMut(),
{
    let start = Instant::now();
    for _ in 0..iterations {
        f();
    }
    let elapsed = start.elapsed();
    let ns_total = elapsed.as_nanos() as f64;
    let ms = ns_total / 1_000_000.0;
    let per = ns_total / iterations as f64;
    println!("  {}: {:.2}ms total, {:.1}ns/op", name, ms, per);
}

fn list_from_values(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}
