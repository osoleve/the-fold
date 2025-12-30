//! fold-daemon — Persistent REPL Daemon
//!
//! A file-based IPC system for The Fold.
//! Polls for request files, evaluates them, writes responses.
//!
//! Protocol:
//!   .fold-repl/
//!     requests/<session-id>.ss   — Client writes expressions here
//!     responses/<session-id>.txt — Daemon writes results here
//!     ready                      — Sentinel file (exists = daemon running)
//!
//! Usage:
//!   Start:  ./fold-daemon
//!   Stop:   Delete .fold-repl/ready

use std::collections::HashMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use fold_rs::fabric::{Env, EnvRef, EvalOutcome, Symbol, Value, eval_spanned};
use fold_rs::tools::fold_parse::Sexp;
use fold_rs::tools::{format_value, lower_program, parse_fold_program};

const REPL_DIR: &str = ".fold-repl";
const REQUESTS_DIR: &str = ".fold-repl/requests";
const RESPONSES_DIR: &str = ".fold-repl/responses";
const READY_FILE: &str = ".fold-repl/ready";
const POLL_INTERVAL_MS: u64 = 100;
const DEFAULT_FUEL: usize = 10_000;

/// Check if an S-expression is a define form: (define name value)
fn is_define_form(sexp: &Sexp) -> bool {
    matches!(sexp, Sexp::List(items) if items.len() == 3 &&
        matches!(&items[0].value, Sexp::Symbol(s) if s == "define"))
}

/// Handle a define form: extract name, evaluate value, bind in environment
fn handle_define_form(sexp: &Sexp, env: &EnvRef) -> Result<String, String> {
    if let Sexp::List(items) = sexp {
        if items.len() != 3 {
            return Err("define expects 2 arguments: (define name value)".to_string());
        }

        // Extract the name
        let name = match &items[1].value {
            Sexp::Symbol(sym) => Symbol::from(sym.as_str()),
            _ => return Err("define: first argument must be a symbol".to_string()),
        };

        // Lower and evaluate the value
        let value_sexp = &items[2];
        match fold_rs::tools::fold_lower::lower_expr(value_sexp) {
            Ok(expr) => {
                match eval_spanned(expr, env.clone(), DEFAULT_FUEL) {
                    Ok(EvalOutcome::Done(value)) => {
                        // Bind the value in the session environment
                        Env::insert(env, name.clone(), value);
                        Ok(format!("; defined {}", name))
                    }
                    Ok(EvalOutcome::Suspended { .. }) => {
                        Err("[define: suspended - out of fuel]".to_string())
                    }
                    Err(err) => Err(format!("define: {}", err)),
                }
            }
            Err(err) => Err(format!("define: {}", err)),
        }
    } else {
        Err("define expects a list form".to_string())
    }
}

/// Check if an S-expression is a load form: (load "path")
fn is_load_form(sexp: &Sexp) -> bool {
    matches!(sexp, Sexp::List(items) if items.len() == 2 &&
        matches!(&items[0].value, Sexp::Symbol(s) if s == "load"))
}

/// Handle a load form: load file, evaluate in current environment
fn handle_load_form(sexp: &Sexp, env: &EnvRef, _session_id: &str) -> Result<String, String> {
    if let Sexp::List(items) = sexp {
        if items.len() != 2 {
            return Err("load expects 1 argument: (load \"path\")".to_string());
        }

        // Extract the path
        let path = match &items[1].value {
            Sexp::String(s) => s.clone(),
            _ => return Err("load: argument must be a string".to_string()),
        };

        // Read and parse the file
        let source = fs::read_to_string(&path)
            .map_err(|err| format!("load: failed to read {}: {}", path, err))?;

        let parsed = parse_fold_program(&source, Some(&path))
            .map_err(|err| format!("load: parse error in {}: {}", path, err))?;

        // Process the parsed expressions, handling define and load forms specially
        let mut remaining = Vec::new();

        for spanned_sexp in parsed {
            if is_define_form(&spanned_sexp.value) {
                // Handle define form
                match handle_define_form(&spanned_sexp.value, env) {
                    Ok(_msg) => {
                        // Define succeeded
                    }
                    Err(e) => return Err(format!("load: {} in {}", e, path)),
                }
            } else if is_load_form(&spanned_sexp.value) {
                // Handle nested load form (recursive)
                match handle_load_form(&spanned_sexp.value, env, _session_id) {
                    Ok(_msg) => {
                        // Nested load succeeded
                    }
                    Err(e) => return Err(format!("load: {} in {}", e, path)),
                }
            } else {
                // Keep non-define/non-load expressions for normal evaluation
                remaining.push(spanned_sexp);
            }
        }

        // Lower and evaluate remaining expressions
        if !remaining.is_empty() {
            let exprs = lower_program(&remaining)
                .map_err(|err| format!("load: lower error in {}: {}", path, err))?;

            for expr in exprs {
                match eval_spanned(expr, env.clone(), DEFAULT_FUEL) {
                    Ok(EvalOutcome::Done(_value)) => {
                        // Expression evaluated successfully, continue
                    }
                    Ok(EvalOutcome::Suspended { .. }) => {
                        return Err(format!("load: suspended during {}", path));
                    }
                    Err(err) => {
                        return Err(format!("load: error in {}: {}", path, err));
                    }
                }
            }
        }

        Ok(format!("; loaded {}", path))
    } else {
        Err("load expects a list form".to_string())
    }
}

/// Session state for a connected client.
struct Session {
    env: EnvRef,
    last_active: Instant,
}

impl Session {
    fn new(_id: String) -> Self {
        let env = Env::new();

        // With the lowerer now recognizing builtin primitives,
        // we don't need to pre-populate the environment with primitive closures.
        // Primitives are automatically converted to Prim expressions during lowering.

        Self {
            env,
            last_active: Instant::now(),
        }
    }

    fn touch(&mut self) {
        self.last_active = Instant::now();
    }
}

/// The daemon state.
struct Daemon {
    sessions: HashMap<String, Session>,
}

impl Daemon {
    fn new() -> Self {
        Self {
            sessions: HashMap::new(),
        }
    }

    /// Get or create a session.
    fn get_or_create_session(&mut self, session_id: &str) -> &mut Session {
        if !self.sessions.contains_key(session_id) {
            self.sessions
                .insert(session_id.to_string(), Session::new(session_id.to_string()));
        }
        self.sessions.get_mut(session_id).unwrap()
    }

    /// Evaluate an expression string and return the result.
    fn eval_source(&mut self, session_id: &str, source: &str) -> Result<String, String> {
        let session = self.get_or_create_session(session_id);
        session.touch();

        // Parse the source
        let parsed = parse_fold_program(source, Some(session_id))
            .map_err(|err| format!("Parse error: {}", err))?;

        // Get the session environment
        let env = session.env.clone();

        // Check for define forms before lowering
        // This allows persistent variable bindings across REPL sessions
        let mut responses = Vec::new();
        let mut remaining = Vec::new();

        for spanned_sexp in parsed {
            if is_define_form(&spanned_sexp.value) {
                match handle_define_form(&spanned_sexp.value, &env) {
                    Ok(msg) => {
                        responses.push(msg);
                    }
                    Err(e) => return Err(e),
                }
            } else if is_load_form(&spanned_sexp.value) {
                match handle_load_form(&spanned_sexp.value, &env, session_id) {
                    Ok(msg) => {
                        responses.push(msg);
                    }
                    Err(e) => return Err(e),
                }
            } else {
                // Keep non-define/non-load expressions for normal evaluation
                remaining.push(spanned_sexp);
            }
        }

        // Lower and evaluate non-define expressions
        let exprs = if remaining.is_empty() {
            Vec::new()
        } else {
            lower_program(&remaining).map_err(|err| format!("Lower error: {}", err))?
        };

        // Check if there are any non-define expressions to evaluate
        let has_other_exprs = !exprs.is_empty();

        // Evaluate each expression, keeping the last result
        let mut last_result = Value::Nil;

        for expr in exprs {
            match eval_spanned(expr, env.clone(), DEFAULT_FUEL) {
                Ok(EvalOutcome::Done(value)) => {
                    last_result = value;
                }
                Ok(EvalOutcome::Suspended { .. }) => {
                    return Ok("[suspended - out of fuel]".to_string());
                }
                Err(err) => {
                    // Error now includes source location if available
                    return Err(format!("{}", err));
                }
            }
        }

        // Combine define responses with evaluation result
        if responses.is_empty() {
            Ok(format!("=> {}", format_value(&last_result)))
        } else if !has_other_exprs {
            // Only define forms, no other expressions
            Ok(responses.join("\n"))
        } else {
            // Mix of define and other expressions
            responses.push(format!("=> {}", format_value(&last_result)));
            Ok(responses.join("\n"))
        }
    }

    /// Process a single request file.
    fn process_request(&mut self, request_path: &Path) {
        // Extract session ID from filename
        let session_id = match request_path.file_stem() {
            Some(stem) => stem.to_string_lossy().to_string(),
            None => return,
        };

        // Read the request
        let source = match fs::read_to_string(request_path) {
            Ok(s) => s,
            Err(err) => {
                eprintln!("[{}] Failed to read request: {}", session_id, err);
                return;
            }
        };

        // Delete the request file immediately
        if let Err(err) = fs::remove_file(request_path) {
            eprintln!("[{}] Failed to remove request file: {}", session_id, err);
        }

        // Evaluate and get response
        let response = match self.eval_source(&session_id, &source) {
            Ok(result) => result,
            Err(err) => format!("ERROR: {}", err),
        };

        // Write response
        let response_path = PathBuf::from(RESPONSES_DIR).join(format!("{}.txt", session_id));
        if let Err(err) = fs::write(&response_path, &response) {
            eprintln!(
                "[{}] Failed to write response to {:?}: {}",
                session_id, response_path, err
            );
        }
    }

    /// Poll for new requests.
    fn poll_requests(&mut self) {
        let requests_path = Path::new(REQUESTS_DIR);
        if !requests_path.exists() {
            return;
        }

        // Read directory entries
        let entries = match fs::read_dir(requests_path) {
            Ok(entries) => entries,
            Err(_) => return,
        };

        // Process each .ss file
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("ss") {
                self.process_request(&path);
            }
        }
    }

    /// Run the daemon loop.
    fn run(&mut self) {
        let poll_interval = Duration::from_millis(POLL_INTERVAL_MS);

        loop {
            // Check if ready file still exists (deletion signals shutdown)
            if !Path::new(READY_FILE).exists() {
                println!("Ready file removed, shutting down...");
                break;
            }

            self.poll_requests();
            thread::sleep(poll_interval);
        }
    }
}

fn ensure_directories() -> std::io::Result<()> {
    fs::create_dir_all(REPL_DIR)?;
    fs::create_dir_all(REQUESTS_DIR)?;
    fs::create_dir_all(RESPONSES_DIR)?;
    Ok(())
}

fn write_ready_file() -> std::io::Result<()> {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let mut file = File::create(READY_FILE)?;
    write!(file, "{}", timestamp)?;
    Ok(())
}

fn clear_ready_file() {
    let _ = fs::remove_file(READY_FILE);
}

fn print_banner() {
    println!("============================================================");
    println!("              THE FOLD - RUST REPL DAEMON                   ");
    println!("============================================================");
    println!("Watching: {}/", REQUESTS_DIR);
    println!("Output:   {}/", RESPONSES_DIR);
    println!("Waiting for requests...");
    println!();
}

fn main() {
    // Ensure directories exist
    if let Err(err) = ensure_directories() {
        eprintln!("Failed to create directories: {}", err);
        std::process::exit(1);
    }

    // Write ready file
    if let Err(err) = write_ready_file() {
        eprintln!("Failed to write ready file: {}", err);
        std::process::exit(1);
    }

    print_banner();

    // Run daemon (stops when ready file is deleted)
    let mut daemon = Daemon::new();
    daemon.run();

    // Cleanup
    clear_ready_file();
    println!("Daemon stopped.");
}
