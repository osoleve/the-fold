use crate::fabric::{
    closure::Closure,
    env::{Env, EnvRef},
    error::{EvalError, SpannedEvalError},
    expr::{CaseArm, Expr, SpannedExpr},
    symbol::Symbol,
    value::Value,
};
use crate::thimble::apply_prim;
use crate::tools::Span;
use crate::tools::fold_load::load_fold_program;
use smallvec::SmallVec;
use std::rc::Rc;

/// SmallVec for evaluated values - inline storage for up to 4 elements
/// to avoid heap allocation for common function calls and let bindings.
type ValueVec = SmallVec<[Value; 4]>;

#[derive(Debug, Clone)]
pub enum EvalOutcome {
    Done(Value),
    Suspended { expr: SpannedExpr, env: EnvRef },
}

#[derive(Debug, Clone)]
enum Frame {
    If {
        then_branch: SpannedExpr,
        else_branch: SpannedExpr,
        env: EnvRef,
        span: Option<Span>,
    },
    Let {
        bindings: Vec<(Symbol, SpannedExpr)>,
        index: usize,
        values: ValueVec,
        body: SpannedExpr,
        env: EnvRef,
        span: Option<Span>,
    },
    LetRec {
        bindings: Vec<(Symbol, SpannedExpr)>,
        index: usize,
        values: ValueVec,
        body: SpannedExpr,
        env: EnvRef,
        span: Option<Span>,
    },
    CallFunc {
        args: Vec<SpannedExpr>,
        env: EnvRef,
        span: Option<Span>,
    },
    CallArgs {
        func: Value,
        args: Vec<SpannedExpr>,
        index: usize,
        values: ValueVec,
        env: EnvRef,
        span: Option<Span>,
    },
    PrimArgs {
        op: Symbol,
        args: Vec<SpannedExpr>,
        index: usize,
        values: ValueVec,
        env: EnvRef,
        span: Option<Span>,
    },
    Case {
        arms: Vec<CaseArm>,
        else_body: Option<Box<SpannedExpr>>,
        env: EnvRef,
        span: Option<Span>,
    },
    /// Load expressions from a file, evaluating them in sequence
    Load {
        /// Remaining expressions to evaluate
        remaining: Vec<SpannedExpr>,
        env: EnvRef,
        span: Option<Span>,
    },
    /// Define a new binding in the environment
    Define {
        name: Symbol,
        env: EnvRef,
        span: Option<Span>,
    },
    /// Exception handler - marks a point in the stack where exceptions should be caught
    ExceptionHandler {
        /// The handler function - takes the condition as an argument
        handler: Value,
        /// The environment to use when calling the handler
        env: EnvRef,
        /// Depth of the frame stack when this handler was installed
        stack_depth: usize,
        span: Option<Span>,
    },
    /// Installing an exception handler - first evaluate the handler, then the body
    InstallHandler {
        body: SpannedExpr,
        env: EnvRef,
        span: Option<Span>,
    },
    /// Set a variable binding
    Set {
        name: Symbol,
        env: EnvRef,
        span: Option<Span>,
    },
}

impl Frame {
    fn env(&self) -> &EnvRef {
        match self {
            Frame::If { env, .. }
            | Frame::Let { env, .. }
            | Frame::LetRec { env, .. }
            | Frame::CallFunc { env, .. }
            | Frame::CallArgs { env, .. }
            | Frame::PrimArgs { env, .. }
            | Frame::Case { env, .. }
            | Frame::Load { env, .. }
            | Frame::Define { env, .. }
            | Frame::ExceptionHandler { env, .. }
            | Frame::InstallHandler { env, .. }
            | Frame::Set { env, .. } => env,
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            Frame::If { span, .. }
            | Frame::Let { span, .. }
            | Frame::LetRec { span, .. }
            | Frame::CallFunc { span, .. }
            | Frame::CallArgs { span, .. }
            | Frame::PrimArgs { span, .. }
            | Frame::Case { span, .. }
            | Frame::Load { span, .. }
            | Frame::Define { span, .. }
            | Frame::ExceptionHandler { span, .. }
            | Frame::InstallHandler { span, .. }
            | Frame::Set { span, .. } => span.clone(),
        }
    }

    fn reify(&self, current: SpannedExpr) -> SpannedExpr {
        match self {
            Frame::If {
                then_branch,
                else_branch,
                span,
                ..
            } => SpannedExpr::new(
                Expr::If {
                    test: Box::new(current),
                    then_branch: Box::new(then_branch.clone()),
                    else_branch: Box::new(else_branch.clone()),
                },
                span.clone(),
            ),
            Frame::Let {
                bindings,
                index,
                values,
                body,
                span,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(bindings.len());
                for (i, (name, expr)) in bindings.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        SpannedExpr::unspanned(Expr::Value(values[i].clone()))
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push((name, expr));
                }
                SpannedExpr::new(
                    Expr::Let {
                        bindings: rebuilt,
                        body: Box::new(body.clone()),
                    },
                    span.clone(),
                )
            }
            Frame::LetRec {
                bindings,
                index,
                values,
                body,
                span,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(bindings.len());
                for (i, (name, expr)) in bindings.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        SpannedExpr::unspanned(Expr::Value(values[i].clone()))
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push((name, expr));
                }
                SpannedExpr::new(
                    Expr::LetRec {
                        bindings: rebuilt,
                        body: Box::new(body.clone()),
                    },
                    span.clone(),
                )
            }
            Frame::CallFunc { args, span, .. } => SpannedExpr::new(
                Expr::Call {
                    func: Box::new(current),
                    args: args.clone(),
                },
                span.clone(),
            ),
            Frame::CallArgs {
                func,
                args,
                index,
                values,
                span,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(args.len());
                for (i, expr) in args.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        SpannedExpr::unspanned(Expr::Value(values[i].clone()))
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push(expr);
                }
                SpannedExpr::new(
                    Expr::Call {
                        func: Box::new(SpannedExpr::unspanned(Expr::Value(func.clone()))),
                        args: rebuilt,
                    },
                    span.clone(),
                )
            }
            Frame::PrimArgs {
                op,
                args,
                index,
                values,
                span,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(args.len());
                for (i, expr) in args.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        SpannedExpr::unspanned(Expr::Value(values[i].clone()))
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push(expr);
                }
                SpannedExpr::new(
                    Expr::Prim {
                        op: op.clone(),
                        args: rebuilt,
                    },
                    span.clone(),
                )
            }
            Frame::Case {
                arms,
                else_body,
                span,
                ..
            } => SpannedExpr::new(
                Expr::Case {
                    expr: Box::new(current),
                    arms: arms.clone(),
                    else_body: else_body.clone(),
                },
                span.clone(),
            ),
            Frame::Load {
                remaining, span, ..
            } => {
                // Reify as a sequence: current followed by remaining expressions
                let mut exprs = vec![current];
                exprs.extend(remaining.iter().cloned());
                // Wrap in nested lets to sequence them
                let mut result = exprs
                    .pop()
                    .unwrap_or_else(|| SpannedExpr::unspanned(Expr::Value(Value::Nil)));
                for (i, expr) in exprs.into_iter().rev().enumerate() {
                    result = SpannedExpr::new(
                        Expr::Let {
                            bindings: vec![(Symbol::intern(&format!("#%load-seq-{i}")), expr)],
                            body: Box::new(result),
                        },
                        span.clone(),
                    );
                }
                result
            }
            Frame::Define { name, span, .. } => SpannedExpr::new(
                Expr::Define {
                    name: name.clone(),
                    value: Box::new(current),
                },
                span.clone(),
            ),
            Frame::ExceptionHandler { .. } => {
                // Exception handlers don't modify the expression structure
                // They just mark a point where exceptions are caught
                current
            }
            Frame::InstallHandler { body, span, .. } => {
                // Reify as: (with-exception-handler current body)
                SpannedExpr::new(
                    Expr::WithExceptionHandler {
                        handler: Box::new(current),
                        body: Box::new(body.clone()),
                    },
                    span.clone(),
                )
            }
            Frame::Set { name, span, .. } => SpannedExpr::new(
                Expr::Set {
                    name: name.clone(),
                    value: Box::new(current),
                },
                span.clone(),
            ),
        }
    }
}

/// Evaluate a spanned expression, returning errors with source locations.
pub fn eval_spanned(
    expr: SpannedExpr,
    env: EnvRef,
    fuel: usize,
) -> Result<EvalOutcome, SpannedEvalError> {
    let mut expr = expr;
    let mut env = env;
    let mut fuel = fuel;
    let mut frames: Vec<Frame> = Vec::new();

    loop {
        if fuel == 0 {
            let suspended_expr = reify(expr, &frames);
            let suspended_env = base_env(&env, &frames);
            return Ok(EvalOutcome::Suspended {
                expr: suspended_expr,
                env: suspended_env,
            });
        }

        fuel -= 1;
        let current_span = expr.span.clone();

        match expr.expr {
            Expr::Value(value) => match unwind(value, &mut frames, &mut env)? {
                Unwind::Continue(next_expr) => {
                    expr = next_expr;
                    continue;
                }
                Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
            },
            Expr::Quote(value) => match unwind(value, &mut frames, &mut env)? {
                Unwind::Continue(next_expr) => {
                    expr = next_expr;
                    continue;
                }
                Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
            },
            Expr::Var(name) => {
                let value = Env::lookup(&env, &name).ok_or_else(|| {
                    SpannedEvalError::new(EvalError::UnboundVariable(name), current_span.clone())
                })?;
                match unwind(value, &mut frames, &mut env)? {
                    Unwind::Continue(next_expr) => {
                        expr = next_expr;
                        continue;
                    }
                    Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                }
            }
            Expr::Fn { params, body } => {
                let value = Value::closure(Closure::new(params, *body, env.clone()));
                match unwind(value, &mut frames, &mut env)? {
                    Unwind::Continue(next_expr) => {
                        expr = next_expr;
                        continue;
                    }
                    Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                }
            }
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => {
                frames.push(Frame::If {
                    then_branch: *then_branch,
                    else_branch: *else_branch,
                    env: env.clone(),
                    span: current_span,
                });
                expr = *test;
            }
            Expr::Let { bindings, body } => {
                if bindings.is_empty() {
                    expr = *body;
                } else {
                    let first_expr = bindings[0].1.clone();
                    frames.push(Frame::Let {
                        bindings,
                        index: 0,
                        values: SmallVec::new(),
                        body: *body,
                        env: env.clone(),
                        span: current_span,
                    });
                    expr = first_expr;
                }
            }
            Expr::LetRec { bindings, body } => {
                if bindings.is_empty() {
                    expr = *body;
                } else {
                    // Create a recursive environment with all binding names
                    let rec_env = Env::with_parent(env.clone());

                    // Evaluate first binding expression in the recursive environment
                    let first_expr = bindings[0].1.clone();
                    frames.push(Frame::LetRec {
                        bindings,
                        index: 0,
                        values: SmallVec::new(),
                        body: *body,
                        env: rec_env.clone(),
                        span: current_span,
                    });
                    env = rec_env;
                    expr = first_expr;
                }
            }
            Expr::Call { func, args } => {
                frames.push(Frame::CallFunc {
                    args,
                    env: env.clone(),
                    span: current_span,
                });
                expr = *func;
            }
            Expr::Prim { op, args } => {
                if args.is_empty() {
                    // Call primitive with no args - check for exceptions
                    let value = match apply_prim(&op, &[]) {
                        Ok(v) => v,
                        Err(EvalError::UncaughtException(condition)) => {
                            // Exception was raised - unwind to nearest handler
                            match unwind_exception(condition, &mut frames, &mut env)? {
                                Unwind::Continue(next_expr) => {
                                    expr = next_expr;
                                    continue;
                                }
                                Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                            }
                        }
                        Err(e) => return Err(SpannedEvalError::new(e, current_span.clone())),
                    };
                    match unwind(value, &mut frames, &mut env)? {
                        Unwind::Continue(next_expr) => {
                            expr = next_expr;
                            continue;
                        }
                        Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                    }
                } else {
                    let first_expr = args[0].clone();
                    frames.push(Frame::PrimArgs {
                        op,
                        args,
                        index: 0,
                        values: SmallVec::new(),
                        env: env.clone(),
                        span: current_span,
                    });
                    expr = first_expr;
                }
            }
            Expr::Fix { name, value } => match value.expr {
                Expr::Fn { params, body } => {
                    let rec_env = Env::with_parent(env.clone());
                    let closure = Rc::new(Closure::new(params, *body, rec_env.clone()));
                    Env::insert(&rec_env, name, Value::Closure(Rc::clone(&closure)));
                    let value = Value::Closure(closure);
                    match unwind(value, &mut frames, &mut env)? {
                        Unwind::Continue(next_expr) => {
                            expr = next_expr;
                            continue;
                        }
                        Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                    }
                }
                _ => {
                    return Err(SpannedEvalError::new(
                        EvalError::FixRequiresFn,
                        current_span,
                    ));
                }
            },
            Expr::Case {
                expr: scrutinee,
                arms,
                else_body,
            } => {
                frames.push(Frame::Case {
                    arms,
                    else_body,
                    env: env.clone(),
                    span: current_span,
                });
                expr = *scrutinee;
            }
            Expr::Load { path } => {
                // Push a frame to handle the result of evaluating path
                frames.push(Frame::Load {
                    remaining: Vec::new(), // Will be populated after path is evaluated
                    env: env.clone(),
                    span: current_span,
                });
                // First evaluate the path expression
                expr = *path;
            }
            Expr::Define { name, value } => {
                // Push a frame to handle the result of evaluating value
                frames.push(Frame::Define {
                    name,
                    env: env.clone(),
                    span: current_span,
                });
                // Evaluate the value expression
                expr = *value;
            }
            Expr::Bound { name } => {
                // Check if the symbol is bound in the current environment
                let is_bound = Env::lookup(&env, &name).is_some();
                let value = Value::Bool(is_bound);
                match unwind(value, &mut frames, &mut env)? {
                    Unwind::Continue(next_expr) => {
                        expr = next_expr;
                        continue;
                    }
                    Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                }
            }
            Expr::WithExceptionHandler { handler, body } => {
                // First evaluate the handler expression to get a closure
                // Then install it as an exception handler frame
                // Then evaluate the body
                frames.push(Frame::InstallHandler {
                    body: *body,
                    env: env.clone(),
                    span: current_span,
                });
                // Evaluate the handler first - when it returns, we'll install it
                expr = *handler;
            }
            Expr::Set { name, value } => {
                // Push a frame to handle the result of evaluating value
                frames.push(Frame::Set {
                    name,
                    env: env.clone(),
                    span: current_span,
                });
                // Evaluate the value expression
                expr = *value;
            }
        }
    }
}

/// Legacy eval_loop that takes bare Expr and wraps it
pub fn eval_loop(expr: Expr, env: EnvRef, fuel: usize) -> Result<EvalOutcome, EvalError> {
    let spanned = SpannedExpr::unspanned(expr);
    eval_spanned(spanned, env, fuel).map_err(|e| e.error)
}

enum Unwind {
    Continue(SpannedExpr),
    Done(Value),
}

fn unwind(
    mut value: Value,
    frames: &mut Vec<Frame>,
    env: &mut EnvRef,
) -> Result<Unwind, SpannedEvalError> {
    loop {
        let frame = match frames.pop() {
            Some(frame) => frame,
            None => return Ok(Unwind::Done(value)),
        };

        let frame_span = frame.span();

        match frame {
            Frame::If {
                then_branch,
                else_branch,
                env: frame_env,
                ..
            } => {
                *env = frame_env;
                let next = if is_truthy(&value) {
                    then_branch
                } else {
                    else_branch
                };
                return Ok(Unwind::Continue(next));
            }
            Frame::Let {
                bindings,
                index,
                mut values,
                body,
                env: frame_env,
                span,
            } => {
                values.push(value);
                if index + 1 < bindings.len() {
                    // Build environment with all bindings collected so far.
                    // We use frame_env as the parent (not current env) because current env
                    // may be polluted by function calls (closures capture their
                    // definition-time env, not the let*'s accumulated env).
                    //
                    // However, we also need to preserve any `define` bindings that were
                    // added during evaluation. Defines use Env::insert to mutate in place,
                    // so they're visible via frame_env's bindings map.
                    let accumulated: Vec<_> = bindings
                        .iter()
                        .take(index + 1)
                        .map(|(name, _)| name.clone())
                        .zip(values.iter().cloned())
                        .collect();
                    let next_env = Env::extend(frame_env.clone(), accumulated);
                    *env = next_env.clone();
                    let next_expr = bindings[index + 1].1.clone();
                    frames.push(Frame::Let {
                        bindings,
                        index: index + 1,
                        values,
                        body,
                        env: next_env, // Use next_env so defines are preserved
                        span,
                    });
                    return Ok(Unwind::Continue(next_expr));
                }

                let mut new_bindings = Vec::with_capacity(bindings.len());
                for (i, (name, _)) in bindings.into_iter().enumerate() {
                    new_bindings.push((name, values[i].clone()));
                }

                // Extend frame_env with all bindings for the body
                *env = Env::extend(frame_env, new_bindings);
                return Ok(Unwind::Continue(body));
            }
            Frame::LetRec {
                bindings,
                index,
                mut values,
                body,
                env: rec_env,
                span,
            } => {
                values.push(value.clone());

                // Insert the current binding into the recursive environment
                // so subsequent bindings can reference it
                let current_name = bindings[index].0.clone();
                Env::insert(&rec_env, current_name, value);

                if index + 1 < bindings.len() {
                    // Continue evaluating the next binding in the recursive environment
                    *env = rec_env.clone();
                    let next_expr = bindings[index + 1].1.clone();
                    frames.push(Frame::LetRec {
                        bindings,
                        index: index + 1,
                        values,
                        body,
                        env: rec_env,
                        span,
                    });
                    return Ok(Unwind::Continue(next_expr));
                }

                // All bindings evaluated - now evaluate the body in the recursive environment
                *env = rec_env;
                return Ok(Unwind::Continue(body));
            }
            Frame::CallFunc {
                args,
                env: frame_env,
                span,
            } => {
                let func = value;
                if args.is_empty() {
                    let (next_expr, next_env) = apply_closure(func, SmallVec::new(), frame_span)?;
                    *env = next_env;
                    return Ok(Unwind::Continue(next_expr));
                }
                *env = frame_env.clone();
                let next_expr = args[0].clone();
                frames.push(Frame::CallArgs {
                    func,
                    args,
                    index: 0,
                    values: SmallVec::new(),
                    env: frame_env,
                    span,
                });
                return Ok(Unwind::Continue(next_expr));
            }
            Frame::CallArgs {
                func,
                args,
                index,
                mut values,
                env: frame_env,
                span,
            } => {
                values.push(value);
                if index + 1 < args.len() {
                    *env = frame_env.clone();
                    let next_expr = args[index + 1].clone();
                    frames.push(Frame::CallArgs {
                        func,
                        args,
                        index: index + 1,
                        values,
                        env: frame_env,
                        span,
                    });
                    return Ok(Unwind::Continue(next_expr));
                }
                let (next_expr, next_env) = apply_closure(func, values, frame_span)?;
                *env = next_env;
                return Ok(Unwind::Continue(next_expr));
            }
            Frame::PrimArgs {
                op,
                args,
                index,
                mut values,
                env: frame_env,
                span,
            } => {
                values.push(value);
                if index + 1 < args.len() {
                    *env = frame_env.clone();
                    let next_expr = args[index + 1].clone();
                    frames.push(Frame::PrimArgs {
                        op,
                        args,
                        index: index + 1,
                        values,
                        env: frame_env,
                        span,
                    });
                    return Ok(Unwind::Continue(next_expr));
                }
                // Call the primitive - if it raises an exception, unwind to handler
                match apply_prim(&op, &values) {
                    Ok(v) => value = v,
                    Err(EvalError::UncaughtException(condition)) => {
                        // Exception was raised - unwind to nearest handler
                        return unwind_exception(condition, frames, env);
                    }
                    Err(e) => return Err(SpannedEvalError::new(e, frame_span)),
                }
            }
            Frame::Case {
                arms,
                else_body,
                env: frame_env,
                ..
            } => {
                let (next_expr, next_env) =
                    apply_case(value, arms, else_body, frame_env, frame_span)?;
                *env = next_env;
                return Ok(Unwind::Continue(next_expr));
            }
            Frame::Load {
                mut remaining,
                env: frame_env,
                span,
            } => {
                // If remaining is empty, we just evaluated the path expression
                if remaining.is_empty() {
                    // Value should be a string path
                    let path = match &value {
                        Value::String(s) => s.clone(),
                        _ => {
                            return Err(SpannedEvalError::new(
                                EvalError::TypeMismatch("load expects a string path"),
                                frame_span,
                            ));
                        }
                    };

                    // Load the file
                    let loaded_exprs = load_file_for_eval(&path).map_err(|e| {
                        SpannedEvalError::new(
                            EvalError::IOError(format!("load: {}", e)),
                            frame_span.clone(),
                        )
                    })?;

                    if loaded_exprs.is_empty() {
                        // Empty file, return Nil
                        value = Value::Nil;
                        // Continue unwinding
                    } else {
                        // Set up to evaluate the loaded expressions
                        let mut exprs = loaded_exprs.into_iter();
                        let first = exprs.next().unwrap();
                        remaining = exprs.collect();

                        if !remaining.is_empty() {
                            // Use current env (which may have been modified by defines)
                            frames.push(Frame::Load {
                                remaining,
                                env: env.clone(),
                                span,
                            });
                        }
                        // Don't override env - keep any defines that were made
                        return Ok(Unwind::Continue(first));
                    }
                } else {
                    // We just finished evaluating one expression from the file
                    // Continue with the next expression, using current env
                    // (which may have been modified by defines in the file)
                    let next_expr = remaining.remove(0);

                    if !remaining.is_empty() {
                        // Use current env for remaining expressions
                        frames.push(Frame::Load {
                            remaining,
                            env: env.clone(),
                            span,
                        });
                    }
                    // Don't restore frame_env - keep current env with any defines
                    let _ = frame_env; // silence unused warning
                    return Ok(Unwind::Continue(next_expr));
                }
            }
            Frame::Define {
                name,
                env: frame_env,
                ..
            } => {
                // Mutate the environment in place so the binding is visible
                // through the parent chain of any environment that references frame_env
                Env::insert(&frame_env, name, value.clone());
                // Keep current env pointing to frame_env
                *env = frame_env;
                // Return the value (like Scheme's define which returns void, but we return the value)
            }
            Frame::ExceptionHandler { .. } => {
                // During normal unwinding, we pass through exception handlers
                // They are only activated when an exception is raised
            }
            Frame::InstallHandler {
                body,
                env: frame_env,
                span,
                ..
            } => {
                // We just finished evaluating the handler expression
                // value should be a closure - install it as an exception handler
                let handler = value;
                // Install the exception handler frame with current stack depth
                let stack_depth = frames.len();
                frames.push(Frame::ExceptionHandler {
                    handler,
                    env: frame_env.clone(),
                    stack_depth,
                    span: span.clone(),
                });
                // Now evaluate the body - wrap in a call if needed
                // The body should be a thunk (zero-argument function), so call it
                *env = frame_env;
                // Wrap body in a Call to invoke it
                let body_call = SpannedExpr::new(
                    Expr::Call {
                        func: Box::new(body),
                        args: vec![],
                    },
                    span,
                );
                return Ok(Unwind::Continue(body_call));
            }
            Frame::Set {
                name,
                env: frame_env,
                span,
            } => {
                // We just finished evaluating the value expression
                // Now set the variable in the environment
                let found = Env::set(&frame_env, &name, value.clone());
                if !found {
                    return Err(SpannedEvalError::new(
                        EvalError::UnboundVariable(name),
                        span,
                    ));
                }
                // Keep the environment as is
                *env = frame_env;
                // Return the new value (R6RS behavior)
            }
        }
    }
}

/// Unwind the frame stack to the nearest exception handler and invoke it.
/// Returns the next expression to evaluate and the environment to use.
fn unwind_exception(
    condition: Value,
    frames: &mut Vec<Frame>,
    env: &mut EnvRef,
) -> Result<Unwind, SpannedEvalError> {
    // Search for the nearest exception handler
    while let Some(frame) = frames.pop() {
        if let Frame::ExceptionHandler {
            handler,
            env: handler_env,
            span,
            ..
        } = frame
        {
            // Found a handler - invoke it with the condition
            *env = handler_env;
            // Call the handler with the condition as argument
            let (next_expr, next_env) =
                apply_closure(handler, SmallVec::from_vec(vec![condition]), span)?;
            *env = next_env;
            return Ok(Unwind::Continue(next_expr));
        }
    }

    // No handler found - return as an uncaught exception
    Err(SpannedEvalError::new(
        EvalError::UncaughtException(condition),
        None,
    ))
}

/// Load a file and return its expressions for evaluation.
fn load_file_for_eval(path: &str) -> Result<Vec<SpannedExpr>, String> {
    load_fold_program(path).map_err(|e| e.to_string())
}

/// Apply a callable (closure or primitive) to arguments.
/// Returns the next expression to evaluate and the environment to use.
fn apply_closure(
    func: Value,
    args: ValueVec,
    span: Option<Span>,
) -> Result<(SpannedExpr, EnvRef), SpannedEvalError> {
    match func {
        Value::Closure(closure) => {
            if closure.params.len() != args.len() {
                return Err(SpannedEvalError::new(
                    EvalError::ArityMismatch {
                        expected: closure.params.len(),
                        got: args.len(),
                    },
                    span,
                ));
            }

            let bindings = closure.params.iter().cloned().zip(args).collect::<Vec<_>>();
            let new_env = Env::extend(closure.env.clone(), bindings);
            Ok(((*closure.body).clone(), new_env))
        }
        Value::Primitive(op) => {
            // Apply primitive and return the result as a Value expression
            let result =
                apply_prim(&op, &args).map_err(|e| SpannedEvalError::new(e, span.clone()))?;
            // Wrap the result in a Value expression so the evaluator can extract it
            Ok((
                SpannedExpr::new(Expr::Value(result), span),
                Env::new(), // Environment doesn't matter for a Value expression
            ))
        }
        _ => Err(SpannedEvalError::new(EvalError::NotCallable, span)),
    }
}

fn apply_case(
    value: Value,
    arms: Vec<CaseArm>,
    else_body: Option<Box<SpannedExpr>>,
    env: EnvRef,
    span: Option<Span>,
) -> Result<(SpannedExpr, EnvRef), SpannedEvalError> {
    let block = match value {
        Value::Block(block) => block,
        _ => return Err(SpannedEvalError::new(EvalError::CaseRequiresBlock, span)),
    };

    for arm in arms {
        if arm.tag == block.tag {
            if arm.vars.len() != block.refs.len() {
                return Err(SpannedEvalError::new(
                    EvalError::PatternArityMismatch {
                        expected: arm.vars.len(),
                        got: block.refs.len(),
                    },
                    span,
                ));
            }

            let bindings = arm
                .vars
                .iter()
                .cloned()
                .zip(block.refs.iter().copied().map(Value::Address))
                .collect::<Vec<_>>();

            let new_env = Env::extend(env, bindings);
            return Ok(((*arm.body).clone(), new_env));
        }
    }

    // No matching arm - check for else clause
    if let Some(else_expr) = else_body {
        return Ok((*else_expr, env));
    }

    Err(SpannedEvalError::new(
        EvalError::NoMatchingClause(block.tag),
        span,
    ))
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn reify(expr: SpannedExpr, frames: &[Frame]) -> SpannedExpr {
    frames
        .iter()
        .rev()
        .fold(expr, |current, frame| frame.reify(current))
}

fn base_env(current: &EnvRef, frames: &[Frame]) -> EnvRef {
    frames
        .first()
        .map(|frame| frame.env().clone())
        .unwrap_or_else(|| current.clone())
}
