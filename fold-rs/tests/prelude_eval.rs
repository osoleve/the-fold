use fold_rs::fabric::{Symbol, env::Env};
use fold_rs::tools::prelude_env;

/// Test that the full prelude can be loaded and evaluated.
/// This is the key test for prelude functionality.
#[test]
fn test_prelude_evaluates() {
    println!("Starting prelude evaluation...");

    // Use a large fuel budget - the prelude has a lot of code
    let fuel = 10_000_000;

    match prelude_env(fuel) {
        Ok(env) => {
            println!("Prelude loaded successfully!");
            // Check that some expected bindings exist

            let map_sym = Symbol::intern("map");
            if Env::lookup(&env, &map_sym).is_some() {
                println!("✓ 'map' is defined");
            } else {
                panic!("'map' should be defined in prelude");
            }

            let filter_sym = Symbol::intern("filter");
            if Env::lookup(&env, &filter_sym).is_some() {
                println!("✓ 'filter' is defined");
            } else {
                panic!("'filter' should be defined in prelude");
            }

            let foldl_sym = Symbol::intern("foldl");
            if Env::lookup(&env, &foldl_sym).is_some() {
                println!("✓ 'foldl' is defined");
            } else {
                panic!("'foldl' should be defined in prelude");
            }
        }
        Err(e) => {
            panic!("Prelude evaluation failed: {}", e);
        }
    }
}
