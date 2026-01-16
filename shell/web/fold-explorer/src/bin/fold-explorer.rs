//! Fold Explorer - Web server for CAS visualization.
//!
//! Usage:
//!   fold-explorer [store-path] [static-dir] [port]
//!
//! Defaults:
//!   store-path: .store
//!   static-dir: static
//!   port: 8080

use std::env;
use std::path::PathBuf;
use std::process;

use fold_explorer::http::Server;
use fold_explorer::store::Store;

fn main() {
    let args: Vec<String> = env::args().collect();

    let store_path = args
        .get(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(".store"));

    let static_path = args
        .get(2)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("static"));

    let port: u16 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(8080);

    println!("Loading store from: {:?}", store_path);
    let store = match Store::open(&store_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to open store: {}", e);
            process::exit(1);
        }
    };

    println!("Indexed {} blocks", store.block_count());
    println!("Static files from: {:?}", static_path);

    let server = Server::new(store, static_path);
    let addr = format!("0.0.0.0:{}", port);

    if let Err(e) = server.listen(&addr) {
        eprintln!("Server error: {}", e);
        process::exit(1);
    }
}
