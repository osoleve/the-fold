//! HTTP server with routing and threading.

use std::io::Write;
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread;
use std::{fs, io};

use crate::graph::{block_list_to_json, block_meta_to_json, popular_to_json, StoreStats, Subgraph};
use crate::json::Json;
use crate::store::Store;

use super::request::Request;
use super::response::Response;

/// The HTTP server.
pub struct Server {
    store: Arc<Store>,
    static_dir: PathBuf,
}

impl Server {
    /// Create a new server with the given store and static directory.
    pub fn new(store: Store, static_dir: impl AsRef<Path>) -> Self {
        Self {
            store: Arc::new(store),
            static_dir: static_dir.as_ref().to_path_buf(),
        }
    }

    /// Start listening for connections.
    pub fn listen(&self, addr: &str) -> io::Result<()> {
        let listener = TcpListener::bind(addr)?;
        println!("Fold Explorer listening on http://{}", addr);

        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let store = Arc::clone(&self.store);
                    let static_dir = self.static_dir.clone();
                    thread::spawn(move || {
                        if let Err(e) = handle_connection(stream, &store, &static_dir) {
                            eprintln!("Connection error: {}", e);
                        }
                    });
                }
                Err(e) => {
                    eprintln!("Accept error: {}", e);
                }
            }
        }
        Ok(())
    }
}

/// Handle a single HTTP connection.
fn handle_connection(mut stream: TcpStream, store: &Store, static_dir: &Path) -> io::Result<()> {
    let request = match Request::parse(&mut stream) {
        Some(req) => req,
        None => {
            let resp = Response::bad_request("invalid request");
            stream.write_all(&resp.to_bytes())?;
            return Ok(());
        }
    };

    let response = route(&request, store, static_dir);
    stream.write_all(&response.to_bytes())?;
    Ok(())
}

/// Route a request to the appropriate handler.
fn route(request: &Request, store: &Store, static_dir: &Path) -> Response {
    let path = &request.path;

    // API routes
    if path.starts_with("/api/") {
        return route_api(request, store);
    }

    // Static files
    serve_static(path, static_dir)
}

/// Route API requests.
fn route_api(request: &Request, store: &Store) -> Response {
    let path = &request.path;

    // GET /api/blocks
    if path == "/api/blocks" {
        let offset = request.query_param_usize("offset", 0);
        let limit = request.query_param_usize("limit", 100).min(1000);
        let tag = request.query_param("tag");
        let prefix = request.query_param("prefix");

        let blocks = store.list_blocks(offset, limit, tag, prefix);
        let mut resp = Response::ok();
        resp.json(&block_list_to_json(&blocks));
        return resp;
    }

    // GET /api/blocks/{hash}
    if path.starts_with("/api/blocks/") && !path.contains("/payload") && !path.contains("/refs") {
        let hash = &path[12..]; // Skip "/api/blocks/"
        return handle_get_block(store, hash);
    }

    // GET /api/blocks/{hash}/payload
    if path.starts_with("/api/blocks/") && path.ends_with("/payload") {
        let hash = &path[12..path.len() - 8]; // Extract hash
        return handle_get_payload(store, hash);
    }

    // GET /api/blocks/{hash}/refs
    if path.starts_with("/api/blocks/") && path.ends_with("/refs") {
        let hash = &path[12..path.len() - 5]; // Extract hash
        return handle_get_refs(store, hash);
    }

    // GET /api/graph/stats
    if path == "/api/graph/stats" {
        let stats = StoreStats::from_store(store);
        let mut resp = Response::ok();
        resp.json(&stats.to_json());
        return resp;
    }

    // GET /api/graph/orphans
    if path == "/api/graph/orphans" {
        let limit = request.query_param_usize("limit", 100).min(1000);
        let orphans = store.get_orphans(limit);
        let mut resp = Response::ok();
        resp.json(&block_list_to_json(&orphans));
        return resp;
    }

    // GET /api/graph/popular
    if path == "/api/graph/popular" {
        let limit = request.query_param_usize("limit", 20).min(100);
        let popular = store.get_popular(limit);
        let mut resp = Response::ok();
        resp.json(&popular_to_json(&popular));
        return resp;
    }

    // GET /api/graph/subgraph/{hash}
    if path.starts_with("/api/graph/subgraph/") {
        let hash = &path[20..];
        let depth = request.query_param_usize("depth", 3).min(10);
        return handle_get_subgraph(store, hash, depth);
    }

    // GET /api/heads
    if path == "/api/heads" {
        return handle_list_heads(store);
    }

    // GET /api/heads/{name}
    if path.starts_with("/api/heads/") {
        let name = &path[11..];
        return handle_get_head(store, name);
    }

    // GET /api/search
    if path == "/api/search" {
        let query = request.query_param("q").unwrap_or("");
        let tag = request.query_param("tag");
        let limit = request.query_param_usize("limit", 50).min(200);

        if query.is_empty() {
            return Response::bad_request("missing query parameter 'q'");
        }

        let results = store.search(query, tag, limit);
        let mut resp = Response::ok();
        resp.json(&block_list_to_json(&results));
        return resp;
    }

    Response::not_found()
}

/// Handle GET /api/blocks/{hash}
fn handle_get_block(store: &Store, hash: &str) -> Response {
    // Validate hash format
    if !is_valid_hash(hash) {
        return Response::bad_request("invalid hash format");
    }

    match store.get_meta(hash) {
        Some(meta) => {
            // Load full block for preview
            let preview = store
                .load_block(hash)
                .map(|b| b.payload_preview(200))
                .unwrap_or_default();

            let json = Json::object(&[
                ("hash", Json::string(&meta.hash)),
                ("tag", Json::string(&meta.tag)),
                ("payload_size", Json::number(meta.payload_size)),
                ("payload_preview", Json::string(&preview)),
                ("refs_count", Json::number(meta.refs_count)),
                ("pinned", Json::bool(meta.pinned)),
            ]);

            let mut resp = Response::ok();
            resp.json(&json);
            resp
        }
        None => Response::not_found(),
    }
}

/// Handle GET /api/blocks/{hash}/payload
fn handle_get_payload(store: &Store, hash: &str) -> Response {
    // Validate hash format
    if !is_valid_hash(hash) {
        return Response::bad_request("invalid hash format");
    }

    match store.get_payload(hash) {
        Ok(payload) => {
            let mut resp = Response::ok();
            // SECURITY: Always serve as octet-stream to prevent XSS
            resp.binary(payload);
            resp
        }
        Err(_) => Response::not_found(),
    }
}

/// Handle GET /api/blocks/{hash}/refs
fn handle_get_refs(store: &Store, hash: &str) -> Response {
    // Validate hash format
    if !is_valid_hash(hash) {
        return Response::bad_request("invalid hash format");
    }

    match store.load_block(hash) {
        Ok(block) => {
            let refs_json: Vec<String> = block
                .refs
                .iter()
                .map(|r| {
                    let ref_hash = r.to_hex();
                    let meta = store.get_meta(&ref_hash);
                    Json::object(&[
                        ("hash", Json::string(&ref_hash)),
                        (
                            "tag",
                            meta.as_ref()
                                .map(|m| Json::string(&m.tag))
                                .unwrap_or_else(Json::null),
                        ),
                        (
                            "payload_size",
                            meta.as_ref()
                                .map(|m| Json::number(m.payload_size))
                                .unwrap_or_else(Json::null),
                        ),
                    ])
                })
                .collect();

            let mut resp = Response::ok();
            resp.json(&Json::array(&refs_json));
            resp
        }
        Err(_) => Response::not_found(),
    }
}

/// Handle GET /api/graph/subgraph/{hash}
fn handle_get_subgraph(store: &Store, hash: &str, depth: usize) -> Response {
    // Validate hash format
    if !is_valid_hash(hash) {
        return Response::bad_request("invalid hash format");
    }

    match store.get_subgraph(hash, depth) {
        Ok((nodes, edges)) => {
            let subgraph = Subgraph { nodes, edges };
            let mut resp = Response::ok();
            resp.json(&subgraph.to_json());
            resp
        }
        Err(e) => Response::internal_error(&format!("{}", e)),
    }
}

/// Handle GET /api/heads
fn handle_list_heads(store: &Store) -> Response {
    match store.list_heads() {
        Ok(heads) => {
            let json: Vec<String> = heads
                .iter()
                .map(|h| {
                    Json::object(&[
                        ("name", Json::string(&h.name)),
                        ("hash", Json::string(&h.hash)),
                    ])
                })
                .collect();

            let mut resp = Response::ok();
            resp.json(&Json::array(&json));
            resp
        }
        Err(e) => Response::internal_error(&format!("{}", e)),
    }
}

/// Handle GET /api/heads/{name}
fn handle_get_head(store: &Store, name: &str) -> Response {
    match store.get_head(name) {
        Ok(head) => {
            let meta = store.get_meta(&head.hash);
            let json = Json::object(&[
                ("name", Json::string(&head.name)),
                ("hash", Json::string(&head.hash)),
                (
                    "block",
                    meta.map(|m| block_meta_to_json(&m))
                        .unwrap_or_else(Json::null),
                ),
            ]);

            let mut resp = Response::ok();
            resp.json(&json);
            resp
        }
        Err(_) => Response::not_found(),
    }
}

/// Serve a static file.
fn serve_static(path: &str, static_dir: &Path) -> Response {
    // Normalize path
    let file_path = if path == "/" || path.is_empty() {
        "index.html"
    } else {
        path.trim_start_matches('/')
    };

    // Security: Prevent path traversal
    if file_path.contains("..") {
        return Response::bad_request("invalid path");
    }

    let full_path = static_dir.join(file_path);

    // Try to read the file
    match fs::read(&full_path) {
        Ok(content) => {
            let mut resp = Response::ok();

            // Determine content type from extension
            let ext = full_path.extension().and_then(|e| e.to_str()).unwrap_or("");

            match ext {
                "html" => {
                    resp.html(&String::from_utf8_lossy(&content));
                }
                "css" => {
                    resp.css(&String::from_utf8_lossy(&content));
                }
                "js" => {
                    resp.javascript(&String::from_utf8_lossy(&content));
                }
                "json" => {
                    resp.json(&String::from_utf8_lossy(&content));
                }
                "svg" => {
                    resp.header("Content-Type", "image/svg+xml");
                    resp.body = content;
                }
                "png" => {
                    resp.header("Content-Type", "image/png");
                    resp.body = content;
                }
                "ico" => {
                    resp.header("Content-Type", "image/x-icon");
                    resp.body = content;
                }
                _ => {
                    resp.text(&String::from_utf8_lossy(&content));
                }
            }

            resp
        }
        Err(_) => Response::not_found(),
    }
}

/// Validate that a string is a valid hex hash (prevents path traversal).
fn is_valid_hash(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_hexdigit())
}
