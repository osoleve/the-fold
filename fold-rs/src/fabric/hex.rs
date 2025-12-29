use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HexError {
    OddLength,
    InvalidDigit { index: usize, ch: char },
    InvalidLength { expected: usize, got: usize },
}

impl fmt::Display for HexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HexError::OddLength => write!(f, "hex string has odd length"),
            HexError::InvalidDigit { index, ch } => {
                write!(f, "invalid hex digit at {index}: {ch}")
            }
            HexError::InvalidLength { expected, got } => {
                write!(f, "invalid hex length: expected {expected}, got {got}")
            }
        }
    }
}

impl std::error::Error for HexError {}

pub fn bytes_to_hex(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = Vec::with_capacity(bytes.len() * 2);
    for &b in bytes {
        out.push(HEX[(b >> 4) as usize]);
        out.push(HEX[(b & 0x0f) as usize]);
    }
    String::from_utf8(out).expect("hex output is valid ASCII")
}

pub fn hex_to_bytes(hex: &str) -> Result<Vec<u8>, HexError> {
    let bytes = hex.as_bytes();
    if !bytes.len().is_multiple_of(2) {
        return Err(HexError::OddLength);
    }
    let mut out = Vec::with_capacity(bytes.len() / 2);
    let mut i = 0;
    while i < bytes.len() {
        let hi = hex_value(bytes[i] as char, i)?;
        let lo = hex_value(bytes[i + 1] as char, i + 1)?;
        out.push((hi << 4) | lo);
        i += 2;
    }
    Ok(out)
}

fn hex_value(ch: char, index: usize) -> Result<u8, HexError> {
    match ch {
        '0'..='9' => Ok((ch as u8) - b'0'),
        'a'..='f' => Ok((ch as u8) - b'a' + 10),
        'A'..='F' => Ok((ch as u8) - b'A' + 10),
        _ => Err(HexError::InvalidDigit { index, ch }),
    }
}
