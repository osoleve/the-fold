use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: Rc<str>, // Use Rc<str> for O(1) cloning
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Span {
    fn new(file: Rc<str>, start: Position, end: Position) -> Self {
        Self {
            file,
            line: start.line,
            column: start.column,
            end_line: end.line,
            end_column: end.column,
        }
    }

    fn point(file: Rc<str>, pos: Position) -> Self {
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
    Rational(String, String), // (numerator, denominator) as strings for BigInt parsing
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Number(NumberLit),
    String(String),
    Symbol(String),
    Bool(bool),
    Char(char),
    Bytevector(Vec<u8>),
    Vector(Vec<Spanned<Sexp>>),
    List(Vec<Spanned<Sexp>>),
    /// Dotted list: (a b . c) where heads=[a, b] and tail=c
    DottedList {
        heads: Vec<Spanned<Sexp>>,
        tail: Box<Spanned<Sexp>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlainSexp {
    Number(NumberLit),
    String(String),
    Symbol(String),
    Bool(bool),
    Char(char),
    Bytevector(Vec<u8>),
    Vector(Vec<PlainSexp>),
    List(Vec<PlainSexp>),
    /// Dotted list: (a b . c) where heads=[a, b] and tail=c
    DottedList {
        heads: Vec<PlainSexp>,
        tail: Box<PlainSexp>,
    },
}

pub fn strip_spans(expr: &Spanned<Sexp>) -> PlainSexp {
    match &expr.value {
        Sexp::Number(n) => PlainSexp::Number(n.clone()),
        Sexp::String(s) => PlainSexp::String(s.clone()),
        Sexp::Symbol(s) => PlainSexp::Symbol(s.clone()),
        Sexp::Bool(b) => PlainSexp::Bool(*b),
        Sexp::Char(c) => PlainSexp::Char(*c),
        Sexp::Bytevector(bytes) => PlainSexp::Bytevector(bytes.clone()),
        Sexp::Vector(items) => PlainSexp::Vector(items.iter().map(strip_spans).collect()),
        Sexp::List(items) => PlainSexp::List(items.iter().map(strip_spans).collect()),
        Sexp::DottedList { heads, tail } => PlainSexp::DottedList {
            heads: heads.iter().map(strip_spans).collect(),
            tail: Box::new(strip_spans(tail)),
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub expected: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error: expected {} at {}",
            self.expected, self.span
        )
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
    file: Rc<str>, // Use Rc<str> for O(1) cloning in spans
}

impl Parser {
    fn new(input: &str, file: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
            file: Rc::from(file),
        }
    }

    fn parse_expr(&mut self) -> Result<Spanned<Sexp>, ParseError> {
        self.skip_whitespace_and_comments();
        let start = self.position();
        let value = match self.peek() {
            None => return Err(self.error_at("expression", start)),
            Some('(') => {
                self.advance();
                self.parse_list(')')?
            }
            Some('[') => {
                self.advance();
                self.parse_list(']')?
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
            Some('#') => self.parse_hash_literal()?,
            Some(c) => {
                // Check for ... (ellipsis) - special symbol in macros
                if c == '.'
                    && self.chars.get(self.index + 1) == Some(&'.')
                    && self.chars.get(self.index + 2) == Some(&'.')
                {
                    self.advance(); // consume first .
                    self.advance(); // consume second .
                    self.advance(); // consume third .
                    Sexp::Symbol("...".to_string())
                } else if let Some(special_float) = self.try_parse_special_float() {
                    Sexp::Number(NumberLit::Float(special_float))
                } else if let Some((number, end_index)) = self.scan_number() {
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

    fn parse_list(&mut self, closing: char) -> Result<Sexp, ParseError> {
        self.skip_whitespace_and_comments();
        let mut items = Vec::new();
        loop {
            match self.peek() {
                Some(c) if c == closing => {
                    self.advance();
                    return Ok(Sexp::List(items));
                }
                Some('.') => {
                    // Check if this is a dotted pair separator
                    // A dot followed by whitespace or closing delimiter is a dotted pair
                    let next = self.chars.get(self.index + 1).copied();
                    if next.is_none()
                        || next == Some(closing)
                        || next.map(|c| c.is_whitespace()).unwrap_or(false)
                    {
                        // This is a dotted pair
                        if items.is_empty() {
                            return Err(self.error("expression before dot"));
                        }
                        self.advance(); // consume '.'
                        self.skip_whitespace_and_comments();

                        // Parse the tail expression
                        let tail = self.parse_expr()?;
                        self.skip_whitespace_and_comments();

                        // Expect closing delimiter
                        match self.peek() {
                            Some(c) if c == closing => {
                                self.advance();
                                return Ok(Sexp::DottedList {
                                    heads: items,
                                    tail: Box::new(tail),
                                });
                            }
                            _ => return Err(self.error(&format!("{} after dotted tail", closing))),
                        }
                    } else {
                        // This is part of a number like .5 or a symbol
                        let expr = self.parse_expr()?;
                        items.push(expr);
                        self.skip_whitespace_and_comments();
                    }
                }
                Some(_) => {
                    let expr = self.parse_expr()?;
                    items.push(expr);
                    self.skip_whitespace_and_comments();
                }
                None => return Err(self.error(&closing.to_string())),
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

    fn parse_hash_literal(&mut self) -> Result<Sexp, ParseError> {
        // Expect '#' prefix
        if self.peek() != Some('#') {
            return Err(self.error("hash literal"));
        }
        self.advance();

        match self.peek() {
            Some('t') | Some('f') => {
                // Boolean: #t or #f
                match self.peek() {
                    Some('t') => {
                        self.advance();
                        Ok(Sexp::Bool(true))
                    }
                    Some('f') => {
                        self.advance();
                        Ok(Sexp::Bool(false))
                    }
                    _ => Err(self.error("boolean")),
                }
            }
            Some('\\') => {
                // Character: #\a, #\newline, etc.
                self.advance();
                self.parse_char()
            }
            Some('u') => {
                // Bytevector: #u8(...)
                self.advance();
                if self.peek() == Some('8') {
                    self.advance();
                    self.skip_whitespace_and_comments();
                    if self.peek() == Some('(') {
                        self.advance();
                        self.parse_bytevector_list()
                    } else {
                        Err(self.error("( after #u8"))
                    }
                } else {
                    Err(self.error("8 after #u"))
                }
            }
            Some('"') => {
                // Bytevector from string: #"abc"
                self.parse_bytevector_string()
            }
            Some('(') => {
                // Vector: #(...)
                self.advance();
                self.parse_vector()
            }
            Some('x') | Some('X') => {
                // Hexadecimal number: #xAB, #xFF, etc.
                self.advance();
                self.parse_radix_number(16)
            }
            Some('o') | Some('O') => {
                // Octal number: #o77, etc.
                self.advance();
                self.parse_radix_number(8)
            }
            Some('b') | Some('B') => {
                // Binary number: #b101, etc.
                self.advance();
                self.parse_radix_number(2)
            }
            _ => Err(self.error("hash literal (expected t, f, \\\\, u, \", (, x, o, or b")),
        }
    }

    fn parse_radix_number(&mut self, radix: u32) -> Result<Sexp, ParseError> {
        let mut digits = String::new();
        let mut negative = false;

        // Handle optional sign
        if self.peek() == Some('-') {
            negative = true;
            self.advance();
        } else if self.peek() == Some('+') {
            self.advance();
        }

        // Collect digits
        while let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() && c.to_digit(radix).is_some() {
                digits.push(c);
                self.advance();
            } else if c.is_alphanumeric() || c == '_' {
                // Invalid digit for this radix
                break;
            } else {
                break;
            }
        }

        if digits.is_empty() {
            return Err(self.error("radix number digits"));
        }

        let value = i64::from_str_radix(&digits, radix)
            .map_err(|_| self.error("valid radix number"))?;

        Ok(Sexp::Number(NumberLit::Integer(if negative { -value } else { value })))
    }

    fn parse_char(&mut self) -> Result<Sexp, ParseError> {
        // After #\ we expect either a single character or a named character
        let start_pos = self.position();

        match self.peek() {
            None => Err(self.error_at("character", start_pos)),
            Some(c) if c.is_alphabetic() => {
                // Could be a named character like "newline" or just a letter
                let mut name = String::new();
                while let Some(ch) = self.peek() {
                    if ch.is_alphabetic() {
                        name.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                }

                // Check for named characters
                let ch = match name.as_str() {
                    "newline" => '\n',
                    "space" => ' ',
                    "tab" => '\t',
                    "return" => '\r',
                    "null" | "nul" => '\0',
                    s if s.len() == 1 => s.chars().next().unwrap(),
                    _ => {
                        return Err(
                            self.error_at(&format!("unknown character name: {}", name), start_pos)
                        );
                    }
                };
                Ok(Sexp::Char(ch))
            }
            Some(c) => {
                // Single character
                self.advance();
                Ok(Sexp::Char(c))
            }
        }
    }

    fn parse_bytevector_list(&mut self) -> Result<Sexp, ParseError> {
        // Parse #u8(n1 n2 n3 ...)
        self.skip_whitespace_and_comments();
        let mut bytes = Vec::new();

        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                Some(')') => {
                    self.advance();
                    return Ok(Sexp::Bytevector(bytes));
                }
                Some(_) => {
                    if let Some((num, end_index)) = self.scan_number() {
                        self.advance_to(end_index);
                        match num {
                            NumberLit::Integer(n) if (0..=255).contains(&n) => {
                                bytes.push(n as u8);
                            }
                            _ => return Err(self.error("byte value (0-255)")),
                        }
                    } else {
                        return Err(self.error("byte value"));
                    }
                }
                None => return Err(self.error(")")),
            }
        }
    }

    fn parse_bytevector_string(&mut self) -> Result<Sexp, ParseError> {
        // Parse #"string" as bytevector
        let s = self.parse_string()?;
        Ok(Sexp::Bytevector(s.into_bytes()))
    }

    fn parse_vector(&mut self) -> Result<Sexp, ParseError> {
        // Parse #(expr1 expr2 ...)
        self.skip_whitespace_and_comments();
        let mut items = Vec::new();

        loop {
            match self.peek() {
                Some(')') => {
                    self.advance();
                    return Ok(Sexp::Vector(items));
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

    /// Try to parse special float literals: +inf.0, -inf.0, +nan.0, -nan.0
    fn try_parse_special_float(&mut self) -> Option<f64> {
        let remaining: String = self.chars[self.index..].iter().collect();

        let specials = [
            ("+inf.0", f64::INFINITY),
            ("-inf.0", f64::NEG_INFINITY),
            ("+nan.0", f64::NAN),
            ("-nan.0", f64::NAN),
        ];

        for (literal, value) in specials {
            if remaining.starts_with(literal) {
                // Check that it's followed by a delimiter
                let next_char = remaining.chars().nth(literal.len());
                if next_char.is_none()
                    || next_char == Some(')')
                    || next_char == Some(']')
                    || next_char.map(|c| c.is_whitespace()).unwrap_or(false)
                {
                    for _ in 0..literal.len() {
                        self.advance();
                    }
                    return Some(value);
                }
            }
        }

        None
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
        // Check for rational number (e.g., 1/2, 3/4)
        if idx < len && self.chars[idx] == '/' {
            let rational_start = idx;
            idx += 1; // skip '/'
            let denom_start = idx;
            // Optional sign for denominator
            if idx < len && (self.chars[idx] == '+' || self.chars[idx] == '-') {
                idx += 1;
            }
            // Parse denominator digits
            let denom_digits_start = idx;
            while idx < len && self.chars[idx].is_ascii_digit() {
                idx += 1;
            }
            if idx > denom_digits_start {
                // Valid rational number
                let numer: String = self.chars[self.index..rational_start].iter().collect();
                let denom: String = self.chars[denom_start..idx].iter().collect();
                return Some((NumberLit::Rational(numer, denom), idx));
            } else {
                // Invalid rational, revert
                idx = rational_start;
            }
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
            literal
                .parse::<f64>()
                .ok()
                .map(|n| (NumberLit::Float(n), idx))
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
    // Support ASCII alphabetic and most Unicode letters/symbols
    c.is_alphabetic()
        || matches!(
            c,
            '!' | '$'
                | '%'
                | '&'
                | '*'
                | '/'
                | ':'
                | '<'
                | '='
                | '>'
                | '?'
                | '^'
                | '_'
                | '~'
                | '+'
                | '-'
        )
        // Support common mathematical/logical Unicode symbols
        || matches!(c, 'λ' | 'α'..='ω' | 'Α'..='Ω' | '∧' | '∨' | '¬' | '→' | '←' | '↔' | '∀' | '∃' | '∈' | '∉' | '⊂' | '⊃' | '⊆' | '⊇' | '∅' | '∪' | '∩' | '×' | '÷' | '±' | '∞' | '≤' | '≥' | '≠' | '≡' | '≈')
        // Superscripts and subscripts (Unicode superscripts and Latin-1 supplement)
        || matches!(c, '⁰'..='⁹' | '₀'..='₉' | '¹' | '²' | '³')
        // Square root, other math
        || matches!(c, '√' | '∛' | '∜' | '∑' | '∏' | '∫' | '∂' | '∇' | '∆' | '∝' | '∘')
}

fn is_symbol_subsequent(c: char) -> bool {
    is_symbol_initial(c) || c.is_ascii_digit() || matches!(c, '.' | '@')
}
