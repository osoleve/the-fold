use crate::fabric::{
    closure::Closure,
    env::{Env, EnvRef},
    error::EvalError,
    expr::{CaseArm, Expr},
    symbol::Symbol,
    value::Value,
};
use crate::thimble::apply_prim;

#[derive(Debug, Clone)]
pub enum EvalOutcome {
    Done(Value),
    Suspended { expr: Expr, env: EnvRef },
}

#[derive(Debug, Clone)]
enum Frame {
    If {
        then_branch: Expr,
        else_branch: Expr,
        env: EnvRef,
    },
    Let {
        bindings: Vec<(Symbol, Expr)>,
        index: usize,
        values: Vec<Value>,
        body: Expr,
        env: EnvRef,
    },
    CallFunc {
        args: Vec<Expr>,
        env: EnvRef,
    },
    CallArgs {
        func: Value,
        args: Vec<Expr>,
        index: usize,
        values: Vec<Value>,
        env: EnvRef,
    },
    PrimArgs {
        op: Symbol,
        args: Vec<Expr>,
        index: usize,
        values: Vec<Value>,
        env: EnvRef,
    },
    Case {
        arms: Vec<CaseArm>,
        else_body: Option<Box<Expr>>,
        env: EnvRef,
    },
}

impl Frame {
    fn env(&self) -> &EnvRef {
        match self {
            Frame::If { env, .. }
            | Frame::Let { env, .. }
            | Frame::CallFunc { env, .. }
            | Frame::CallArgs { env, .. }
            | Frame::PrimArgs { env, .. }
            | Frame::Case { env, .. } => env,
        }
    }

    fn reify(&self, current: Expr) -> Expr {
        match self {
            Frame::If {
                then_branch,
                else_branch,
                ..
            } => Expr::If {
                test: Box::new(current),
                then_branch: Box::new(then_branch.clone()),
                else_branch: Box::new(else_branch.clone()),
            },
            Frame::Let {
                bindings,
                index,
                values,
                body,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(bindings.len());
                for (i, (name, expr)) in bindings.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        Expr::Value(values[i].clone())
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push((name, expr));
                }
                Expr::Let {
                    bindings: rebuilt,
                    body: Box::new(body.clone()),
                }
            }
            Frame::CallFunc { args, .. } => Expr::Call {
                func: Box::new(current),
                args: args.clone(),
            },
            Frame::CallArgs {
                func,
                args,
                index,
                values,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(args.len());
                for (i, expr) in args.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        Expr::Value(values[i].clone())
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push(expr);
                }
                Expr::Call {
                    func: Box::new(Expr::Value(func.clone())),
                    args: rebuilt,
                }
            }
            Frame::PrimArgs {
                op,
                args,
                index,
                values,
                ..
            } => {
                let mut rebuilt = Vec::with_capacity(args.len());
                for (i, expr) in args.iter().cloned().enumerate() {
                    let expr = if i < *index {
                        Expr::Value(values[i].clone())
                    } else if i == *index {
                        current.clone()
                    } else {
                        expr
                    };
                    rebuilt.push(expr);
                }
                Expr::Prim {
                    op: op.clone(),
                    args: rebuilt,
                }
            }
            Frame::Case { arms, else_body, .. } => Expr::Case {
                expr: Box::new(current),
                arms: arms.clone(),
                else_body: else_body.clone(),
            },
        }
    }
}

pub fn eval_loop(expr: Expr, env: EnvRef, fuel: usize) -> Result<EvalOutcome, EvalError> {
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

        match expr {
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
                let value = Env::lookup(&env, &name).ok_or(EvalError::UnboundVariable(name))?;
                match unwind(value, &mut frames, &mut env)? {
                    Unwind::Continue(next_expr) => {
                        expr = next_expr;
                        continue;
                    }
                    Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                }
            }
            Expr::Fn { params, body } => {
                let closure = Closure::new(params, *body, env.clone());
                let value = Value::Closure(closure);
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
                        values: Vec::new(),
                        body: *body,
                        env: env.clone(),
                    });
                    expr = first_expr;
                }
            }
            Expr::Call { func, args } => {
                frames.push(Frame::CallFunc {
                    args,
                    env: env.clone(),
                });
                expr = *func;
            }
            Expr::Prim { op, args } => {
                if args.is_empty() {
                    let value = apply_prim(&op, &[])?;
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
                        values: Vec::new(),
                        env: env.clone(),
                    });
                    expr = first_expr;
                }
            }
            Expr::Fix { name, value } => match *value {
                Expr::Fn { params, body } => {
                    let rec_env = Env::with_parent(env.clone());
                    let closure = Closure::new(params, *body, rec_env.clone());
                    Env::insert(&rec_env, name, Value::Closure(closure.clone()));
                    let value = Value::Closure(closure);
                    match unwind(value, &mut frames, &mut env)? {
                        Unwind::Continue(next_expr) => {
                            expr = next_expr;
                            continue;
                        }
                        Unwind::Done(value) => return Ok(EvalOutcome::Done(value)),
                    }
                }
                _ => return Err(EvalError::FixRequiresFn),
            },
            Expr::Case { expr: scrutinee, arms, else_body } => {
                frames.push(Frame::Case {
                    arms,
                    else_body,
                    env: env.clone(),
                });
                expr = *scrutinee;
            }
        }
    }
}

enum Unwind {
    Continue(Expr),
    Done(Value),
}

fn unwind(
    mut value: Value,
    frames: &mut Vec<Frame>,
    env: &mut EnvRef,
) -> Result<Unwind, EvalError> {
    loop {
        let frame = match frames.pop() {
            Some(frame) => frame,
            None => return Ok(Unwind::Done(value)),
        };

        match frame {
            Frame::If {
                then_branch,
                else_branch,
                env: frame_env,
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
            } => {
                values.push(value);
                if index + 1 < bindings.len() {
                    *env = frame_env.clone();
                    let next_expr = bindings[index + 1].1.clone();
                    frames.push(Frame::Let {
                        bindings,
                        index: index + 1,
                        values,
                        body,
                        env: frame_env.clone(),
                    });
                    return Ok(Unwind::Continue(next_expr));
                }

                let mut new_bindings = Vec::with_capacity(bindings.len());
                for (i, (name, _)) in bindings.into_iter().enumerate() {
                    new_bindings.push((name, values[i].clone()));
                }

                *env = Env::extend(frame_env, new_bindings);
                return Ok(Unwind::Continue(body));
            }
            Frame::CallFunc { args, env: frame_env } => {
                let func = value;
                if args.is_empty() {
                    let (next_expr, next_env) = apply_closure(func, Vec::new())?;
                    *env = next_env;
                    return Ok(Unwind::Continue(next_expr));
                }
                *env = frame_env.clone();
                let next_expr = args[0].clone();
                frames.push(Frame::CallArgs {
                    func,
                    args,
                    index: 0,
                    values: Vec::new(),
                    env: frame_env,
                });
                return Ok(Unwind::Continue(next_expr));
            }
            Frame::CallArgs {
                func,
                args,
                index,
                mut values,
                env: frame_env,
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
                    });
                    return Ok(Unwind::Continue(next_expr));
                }
                let (next_expr, next_env) = apply_closure(func, values)?;
                *env = next_env;
                return Ok(Unwind::Continue(next_expr));
            }
            Frame::PrimArgs {
                op,
                args,
                index,
                mut values,
                env: frame_env,
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
                    });
                    return Ok(Unwind::Continue(next_expr));
                }
                value = apply_prim(&op, &values)?;
            }
            Frame::Case { arms, else_body, env: frame_env } => {
                let (next_expr, next_env) = apply_case(value, arms, else_body, frame_env)?;
                *env = next_env;
                return Ok(Unwind::Continue(next_expr));
            }
        }
    }
}

fn apply_closure(func: Value, args: Vec<Value>) -> Result<(Expr, EnvRef), EvalError> {
    let closure = match func {
        Value::Closure(closure) => closure,
        _ => return Err(EvalError::NotCallable),
    };

    if closure.params.len() != args.len() {
        return Err(EvalError::ArityMismatch {
            expected: closure.params.len(),
            got: args.len(),
        });
    }

    let bindings = closure
        .params
        .iter()
        .cloned()
        .zip(args.into_iter())
        .collect::<Vec<_>>();

    let new_env = Env::extend(closure.env.clone(), bindings);
    Ok(((*closure.body).clone(), new_env))
}

fn apply_case(
    value: Value,
    arms: Vec<CaseArm>,
    else_body: Option<Box<Expr>>,
    env: EnvRef,
) -> Result<(Expr, EnvRef), EvalError> {
    let block = match value {
        Value::Block(block) => block,
        _ => return Err(EvalError::CaseRequiresBlock),
    };

    for arm in arms {
        if arm.tag == block.tag {
            if arm.vars.len() != block.refs.len() {
                return Err(EvalError::PatternArityMismatch {
                    expected: arm.vars.len(),
                    got: block.refs.len(),
                });
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

    Err(EvalError::NoMatchingClause(block.tag))
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn reify(expr: Expr, frames: &[Frame]) -> Expr {
    frames.iter().rev().fold(expr, |current, frame| frame.reify(current))
}

fn base_env(current: &EnvRef, frames: &[Frame]) -> EnvRef {
    frames.first().map(|frame| frame.env().clone()).unwrap_or_else(|| current.clone())
}
