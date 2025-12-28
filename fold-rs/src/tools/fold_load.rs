use std::{
    fmt,
    fs,
    path::{Path, PathBuf},
};

use crate::fabric::Expr;
use crate::tools::fold_lower::{lower_expr, lower_program, LowerError};
use crate::tools::fold_parse::{parse_fold_expr, parse_fold_program, ParseError};

#[derive(Debug)]
pub enum LoadError {
    Io { path: PathBuf, source: std::io::Error },
    Parse { path: PathBuf, source: ParseError },
    Lower { path: PathBuf, source: LowerError },
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Io { path, source } => write!(f, "failed to read {}: {}", path.display(), source),
            LoadError::Parse { path, source } => {
                write!(f, "failed to parse {}: {}", path.display(), source)
            }
            LoadError::Lower { path, source } => {
                write!(f, "failed to lower {}: {}", path.display(), source)
            }
        }
    }
}

impl std::error::Error for LoadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LoadError::Io { source, .. } => Some(source),
            LoadError::Parse { source, .. } => Some(source),
            LoadError::Lower { source, .. } => Some(source),
        }
    }
}

pub fn load_fold_program<P: AsRef<Path>>(path: P) -> Result<Vec<Expr>, LoadError> {
    let path = path.as_ref();
    let source = fs::read_to_string(path).map_err(|err| LoadError::Io {
        path: path.to_path_buf(),
        source: err,
    })?;
    let file = path.to_string_lossy().to_string();
    let parsed = parse_fold_program(&source, Some(&file)).map_err(|err| LoadError::Parse {
        path: path.to_path_buf(),
        source: err,
    })?;
    lower_program(&parsed).map_err(|err| LoadError::Lower {
        path: path.to_path_buf(),
        source: err,
    })
}

pub fn load_fold_expr<P: AsRef<Path>>(path: P) -> Result<Expr, LoadError> {
    let path = path.as_ref();
    let source = fs::read_to_string(path).map_err(|err| LoadError::Io {
        path: path.to_path_buf(),
        source: err,
    })?;
    let file = path.to_string_lossy().to_string();
    let parsed = parse_fold_expr(&source, Some(&file)).map_err(|err| LoadError::Parse {
        path: path.to_path_buf(),
        source: err,
    })?;
    lower_expr(&parsed).map_err(|err| LoadError::Lower {
        path: path.to_path_buf(),
        source: err,
    })
}
