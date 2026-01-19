//! Block parsing for The Fold's content-addressed store.
//!
//! Binary format (all lengths little-endian u32):
//!   [tag-len : 4 bytes][tag : tag-len bytes (UTF-8)]
//!   [payload-len : 4 bytes][payload : payload-len bytes]
//!   [refs-count : 4 bytes][ref_0 : N bytes]...[ref_n : N bytes]
//!
//! CRITICAL: Ref size is DYNAMIC (32 or 33 bytes per block).
//! Parser must detect ref size by: remaining_bytes / refs_count

use std::fmt;

/// Size of a SHA-256 hash in bytes.
pub const HASH_SIZE: usize = 32;

/// Size of a versioned address (version byte + hash).
pub const ADDRESS_SIZE: usize = 33;

/// Error type for block parsing.
#[derive(Debug)]
pub enum ParseError {
    TooShort { expected: usize, actual: usize },
    InvalidUtf8(std::string::FromUtf8Error),
    InvalidRefSize { remaining: usize, count: usize },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooShort { expected, actual } => {
                write!(f, "data too short: expected {expected}, got {actual}")
            }
            Self::InvalidUtf8(e) => write!(f, "invalid UTF-8 in tag: {e}"),
            Self::InvalidRefSize { remaining, count } => {
                write!(
                    f,
                    "invalid ref size: {remaining} bytes remaining for {count} refs"
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl From<std::string::FromUtf8Error> for ParseError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::InvalidUtf8(e)
    }
}

/// A block address (33 bytes: version + SHA-256 hash).
/// For legacy 32-byte refs, version is set to 0.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Address([u8; ADDRESS_SIZE]);

impl Address {
    /// Create an address from raw bytes (must be exactly 33 bytes).
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        if bytes.len() == ADDRESS_SIZE {
            let mut arr = [0u8; ADDRESS_SIZE];
            arr.copy_from_slice(bytes);
            Some(Self(arr))
        } else {
            None
        }
    }

    /// Create an address from a legacy 32-byte hash (prepends version 0).
    pub fn from_legacy_hash(hash: &[u8]) -> Option<Self> {
        if hash.len() == HASH_SIZE {
            let mut arr = [0u8; ADDRESS_SIZE];
            arr[0] = 0; // version byte
            arr[1..].copy_from_slice(hash);
            Some(Self(arr))
        } else {
            None
        }
    }

    /// Get the version byte.
    pub fn version(&self) -> u8 {
        self.0[0]
    }

    /// Get the hash bytes (without version).
    pub fn hash_bytes(&self) -> &[u8] {
        &self.0[1..]
    }

    /// Get the raw bytes.
    pub fn as_bytes(&self) -> &[u8; ADDRESS_SIZE] {
        &self.0
    }

    /// Convert to hex string.
    pub fn to_hex(&self) -> String {
        self.0.iter().map(|b| format!("{b:02x}")).collect()
    }

    /// Parse from hex string (must be exactly 66 hex chars for 33 bytes).
    pub fn from_hex(s: &str) -> Option<Self> {
        if s.len() != ADDRESS_SIZE * 2 {
            return None;
        }
        let mut arr = [0u8; ADDRESS_SIZE];
        for (i, chunk) in s.as_bytes().chunks(2).enumerate() {
            let hex_str = std::str::from_utf8(chunk).ok()?;
            arr[i] = u8::from_str_radix(hex_str, 16).ok()?;
        }
        Some(Self(arr))
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Address({})", &self.to_hex()[..16])
    }
}

/// A parsed block from the content-addressed store.
#[derive(Debug, Clone)]
pub struct Block {
    /// The block's tag (type identifier).
    pub tag: String,
    /// The raw payload bytes.
    pub payload: Vec<u8>,
    /// References to other blocks.
    pub refs: Vec<Address>,
    /// Detected ref size (32 or 33 bytes).
    pub ref_size: usize,
}

impl Block {
    /// Parse a block from its binary representation.
    ///
    /// Format: [tag-len:4][tag][payload-len:4][payload][refs-count:4][refs...]
    pub fn from_bytes(data: &[u8]) -> Result<Self, ParseError> {
        let mut pos = 0;

        // Read tag length (little-endian u32)
        if data.len() < pos + 4 {
            return Err(ParseError::TooShort {
                expected: pos + 4,
                actual: data.len(),
            });
        }
        let tag_len =
            u32::from_le_bytes([data[pos], data[pos + 1], data[pos + 2], data[pos + 3]]) as usize;
        pos += 4;

        // Read tag (UTF-8)
        if data.len() < pos + tag_len {
            return Err(ParseError::TooShort {
                expected: pos + tag_len,
                actual: data.len(),
            });
        }
        let tag = String::from_utf8(data[pos..pos + tag_len].to_vec())?;
        pos += tag_len;

        // Read payload length
        if data.len() < pos + 4 {
            return Err(ParseError::TooShort {
                expected: pos + 4,
                actual: data.len(),
            });
        }
        let payload_len =
            u32::from_le_bytes([data[pos], data[pos + 1], data[pos + 2], data[pos + 3]]) as usize;
        pos += 4;

        // Read payload
        if data.len() < pos + payload_len {
            return Err(ParseError::TooShort {
                expected: pos + payload_len,
                actual: data.len(),
            });
        }
        let payload = data[pos..pos + payload_len].to_vec();
        pos += payload_len;

        // Read refs count
        if data.len() < pos + 4 {
            return Err(ParseError::TooShort {
                expected: pos + 4,
                actual: data.len(),
            });
        }
        let refs_count =
            u32::from_le_bytes([data[pos], data[pos + 1], data[pos + 2], data[pos + 3]]) as usize;
        pos += 4;

        // CRITICAL: Detect ref size dynamically (32 or 33 bytes)
        let remaining = data.len() - pos;
        let ref_size = if refs_count == 0 {
            ADDRESS_SIZE // Default to 33 for blocks with no refs
        } else {
            let detected = remaining / refs_count;
            if detected != HASH_SIZE && detected != ADDRESS_SIZE {
                return Err(ParseError::InvalidRefSize {
                    remaining,
                    count: refs_count,
                });
            }
            detected
        };

        // Parse refs
        let mut refs = Vec::with_capacity(refs_count);
        for _ in 0..refs_count {
            if data.len() < pos + ref_size {
                return Err(ParseError::TooShort {
                    expected: pos + ref_size,
                    actual: data.len(),
                });
            }
            let addr = if ref_size == HASH_SIZE {
                // Legacy 32-byte hash: prepend version 0
                Address::from_legacy_hash(&data[pos..pos + ref_size])
            } else {
                // Modern 33-byte address
                Address::from_bytes(&data[pos..pos + ref_size])
            };
            if let Some(addr) = addr {
                refs.push(addr);
            }
            pos += ref_size;
        }

        Ok(Self {
            tag,
            payload,
            refs,
            ref_size,
        })
    }

    /// Get the payload as a UTF-8 string if valid.
    pub fn payload_as_string(&self) -> Option<String> {
        String::from_utf8(self.payload.clone()).ok()
    }

    /// Get a preview of the payload (first N bytes as string or hex).
    pub fn payload_preview(&self, max_len: usize) -> String {
        // Try UTF-8 first
        if let Ok(s) = std::str::from_utf8(&self.payload) {
            if s.len() <= max_len {
                return s.to_string();
            }
            // Find a valid UTF-8 boundary
            let mut end = max_len;
            while end > 0 && !s.is_char_boundary(end) {
                end -= 1;
            }
            return format!("{}...", &s[..end]);
        }
        // Fall back to hex
        let bytes_to_show = max_len.min(self.payload.len()) / 2;
        let hex: String = self.payload[..bytes_to_show]
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect();
        format!("{hex}...")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_roundtrip() {
        let hex = "00".to_string() + &"ab".repeat(32);
        let addr = Address::from_hex(&hex).unwrap();
        assert_eq!(addr.to_hex(), hex);
        assert_eq!(addr.version(), 0);
    }

    #[test]
    fn test_block_parse_minimal() {
        // Minimal block: tag="t", payload="p", no refs
        let mut data = Vec::new();
        // tag-len = 1
        data.extend_from_slice(&1u32.to_le_bytes());
        // tag = "t"
        data.push(b't');
        // payload-len = 1
        data.extend_from_slice(&1u32.to_le_bytes());
        // payload = "p"
        data.push(b'p');
        // refs-count = 0
        data.extend_from_slice(&0u32.to_le_bytes());

        let block = Block::from_bytes(&data).unwrap();
        assert_eq!(block.tag, "t");
        assert_eq!(block.payload, vec![b'p']);
        assert!(block.refs.is_empty());
    }
}
