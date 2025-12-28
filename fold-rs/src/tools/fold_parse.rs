use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Span {
    fn new(file: String, start: Position, end: Position) -> Self {
        Self {
            file,
            line: start.line,
            column: start.column,
            end_line: end.line,
            end_column: end.column,
        }
    }

    fn point(file: String, pos: Position) -> Self {
        Self::new(file, pos, pos)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLit {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Number(NumberLit),
    String(String),
    Symbol(String),
    Bool(bool),
    List(Vec<Spanned<Sexp>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlainSexp {
    Number(NumberLit),
    String(String),
    Symbol(String),
    Bool(bool),
    List(Vec<PlainSexp>),
}

pub fn strip_spans(expr: &Spanned<Sexp>) -> PlainSexp {
    match &expr.value {
        Sexp::Number(n) => PlainSexp::Number(n.clone()),
        Sexp::String(s) => PlainSexp::String(s.clone()),
        Sexp::Symbol(s) => PlainSexp::Symbol(s.clone()),
        Sexp::Bool(b) => PlainSexp::Bool(*b),
        Sexp::List(items) => PlainSexp::List(items.iter().map(strip_spans).collect()),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub expected: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error: expected {} at {}", self.expected, self.span)
    }
}

impl std::error::Error for ParseError {}

pub fn parse_fold_expr(input: &str, file: Option<&str>) -> Result<Spanned<Sexp>, ParseError> {
    let mut parser = Parser::new(input, file.unwrap_or("<input>"));
    parser.parse_expr()
}

pub fn parse_fold_program(
    input: &str,
    file: Option<&str>,
) -> Result<Vec<Spanned<Sexp>>, ParseError> {
    let mut parser = Parser::new(input, file.unwrap_or("<input>"));
    parser.skip_whitespace_and_comments();
    let mut exprs = Vec::new();
    while parser.peek().is_some() {
        exprs.push(parser.parse_expr()?);
        parser.skip_whitespace_and_comments();
    }
    Ok(exprs)
}

struct Parser {
    chars: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
    file: String,
}

impl Parser {
    fn new(input: &str, file: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
            file: file.to_string(),
        }
    }

    fn parse_expr(&mut self) -> Result<Spanned<Sexp>, ParseError> {
        self.skip_whitespace_and_comments();
        let start = self.position();
        let value = match self.peek() {
            None => return Err(self.error_at("expression", start)),
            Some('(') => {
                self.advance();
                self.parse_list()?
            }
            Some('\'') => {
                let quote_pos = self.position();
                self.advance();
                self.parse_quote("quote", quote_pos)?
            }
            Some('`') => {
                let quote_pos = self.position();
                self.advance();
                self.parse_quote("quasiquote", quote_pos)?
            }
            Some(',') => {
                let quote_pos = self.position();
                self.advance();
                if self.peek() == Some('@') {
                    self.advance();
                    self.parse_quote("unquote-splicing", quote_pos)?
                } else {
                    self.parse_quote("unquote", quote_pos)?
                }
            }
            Some('"') => Sexp::String(self.parse_string()?),
            Some('#') => Sexp::Bool(self.parse_boolean()?),
            Some(c) => {
                if let Some((number, end_index)) = self.scan_number() {
                    self.advance_to(end_index);
                    Sexp::Number(number)
                } else if is_symbol_initial(c) {
                    Sexp::Symbol(self.parse_symbol()?)
                } else {
                    return Err(self.error_at("expression", start));
                }
            }
        };
        let end = self.position();
        Ok(Spanned {
            value,
            span: Span::new(self.file.clone(), start, end),
        })
    }

    fn parse_list(&mut self) -> Result<Sexp, ParseError> {
        self.skip_whitespace_and_comments();
        let mut items = Vec::new();
        loop {
            match self.peek() {
                Some(')') => {
                    self.advance();
                    return Ok(Sexp::List(items));
                }
                Some(_) => {
                    let expr = self.parse_expr()?;
                    items.push(expr);
                    self.skip_whitespace_and_comments();
                }
                None => return Err(self.error(")")),
            }
        }
    }

    fn parse_quote(&mut self, keyword: &str, quote_pos: Position) -> Result<Sexp, ParseError> {
        let expr = self.parse_expr()?;
        let symbol = Spanned {
            value: Sexp::Symbol(keyword.to_string()),
            span: Span::point(self.file.clone(), quote_pos),
        };
        Ok(Sexp::List(vec![symbol, expr]))
    }

    fn parse_string(&mut self) -> Result<String, ParseError> {
        self.advance();
        let mut out = String::new();
        loop {
            match self.peek() {
                None => return Err(self.error("end of string")),
                Some('"') => {
                    self.advance();
                    return Ok(out);
                }
                Some('\\') => {
                    self.advance();
                    match self.peek() {
                        None => return Err(self.error("end of string")),
                        Some(c) => {
                            self.advance();
                            let mapped = match c {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '\\' => '\\',
                                '"' => '"',
                                other => other,
                            };
                            out.push(mapped);
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    out.push(c);
                }
            }
        }
    }

    fn parse_boolean(&mut self) -> Result<bool, ParseError> {
        if self.peek() != Some('#') {
            return Err(self.error("boolean"));
        }
        self.advance();
        match self.peek() {
            Some('t') => {
                self.advance();
                Ok(true)
            }
            Some('f') => {
                self.advance();
                Ok(false)
            }
            _ => Err(self.error("boolean")),
        }
    }

    fn parse_symbol(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(c) if is_symbol_initial(c) => {
                let mut out = String::new();
                out.push(c);
                self.advance();
                while let Some(next) = self.peek() {
                    if is_symbol_subsequent(next) {
                        out.push(next);
                        self.advance();
                    } else {
                        break;
                    }
                }
                Ok(out)
            }
            _ => Err(self.error("symbol")),
        }
    }

    fn scan_number(&self) -> Option<(NumberLit, usize)> {
        let len = self.chars.len();
        let mut idx = self.index;
        if idx >= len {
            return None;
        }
        let first = self.chars[idx];
        if first == '+' || first == '-' {
            idx += 1;
        }
        let digits_start = idx;
        while idx < len && self.chars[idx].is_ascii_digit() {
            idx += 1;
        }
        if idx == digits_start {
            return None;
        }
        let mut is_float = false;
        if idx + 1 < len && self.chars[idx] == '.' && self.chars[idx + 1].is_ascii_digit() {
            is_float = true;
            idx += 1;
            while idx < len && self.chars[idx].is_ascii_digit() {
                idx += 1;
            }
        }
        // Check for exponent part (e.g., 1e10, 1.5e-3, 2E+5)
        if idx < len && (self.chars[idx] == 'e' || self.chars[idx] == 'E') {
            let exp_start = idx;
            idx += 1;
            if idx < len && (self.chars[idx] == '+' || self.chars[idx] == '-') {
                idx += 1;
            }
            let exp_digits_start = idx;
            while idx < len && self.chars[idx].is_ascii_digit() {
                idx += 1;
            }
            if idx > exp_digits_start {
                is_float = true; // Scientific notation always produces float
            } else {
                idx = exp_start; // No valid exponent, revert
            }
        }
        let literal: String = self.chars[self.index..idx].iter().collect();
        if is_float {
            literal.parse::<f64>().ok().map(|n| (NumberLit::Float(n), idx))
        } else {
            literal
                .parse::<i64>()
                .ok()
                .map(|n| (NumberLit::Integer(n), idx))
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            let mut progressed = false;
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.advance();
                    progressed = true;
                } else {
                    break;
                }
            }
            if self.peek() == Some(';') {
                progressed = true;
                self.advance();
                while let Some(c) = self.peek() {
                    self.advance();
                    if c == '\n' {
                        break;
                    }
                }
            }
            if !progressed {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn error(&self, expected: &str) -> ParseError {
        let pos = self.position();
        self.error_at(expected, pos)
    }

    fn error_at(&self, expected: &str, pos: Position) -> ParseError {
        ParseError {
            expected: expected.to_string(),
            span: Span::point(self.file.clone(), pos),
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.index += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn advance_to(&mut self, target: usize) {
        while self.index < target {
            self.advance();
        }
    }
}

fn is_symbol_initial(c: char) -> bool {
    c.is_alphabetic()
        || matches!(
            c,
            '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_'
                | '~' | '+' | '-'
        )
}

fn is_symbol_subsequent(c: char) -> bool {
    is_symbol_initial(c) || c.is_ascii_digit() || matches!(c, '.' | '@')
}
