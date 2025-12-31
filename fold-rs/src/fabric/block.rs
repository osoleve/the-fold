use std::fmt;

use crate::fabric::{
    address::{ADDRESS_SIZE, Address},
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub tag: Symbol,
    pub payload: Vec<u8>,
    pub refs: Vec<Address>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockDecodeError {
    Truncated,
    InvalidUtf8,
    TrailingBytes,
}

impl fmt::Display for BlockDecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockDecodeError::Truncated => write!(f, "block bytes truncated"),
            BlockDecodeError::InvalidUtf8 => write!(f, "block tag is not valid UTF-8"),
            BlockDecodeError::TrailingBytes => write!(f, "block bytes have trailing data"),
        }
    }
}

impl std::error::Error for BlockDecodeError {}

impl Block {
    pub fn new(tag: Symbol, payload: Vec<u8>, refs: Vec<Address>) -> Self {
        Self { tag, payload, refs }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let tag_str = self.tag.as_str();
        let tag_bytes = tag_str.as_bytes();
        let mut out = Vec::with_capacity(
            4 + tag_bytes.len() + 4 + self.payload.len() + 4 + self.refs.len() * ADDRESS_SIZE,
        );
        out.extend_from_slice(&(tag_bytes.len() as u32).to_le_bytes());
        out.extend_from_slice(tag_bytes);
        out.extend_from_slice(&(self.payload.len() as u32).to_le_bytes());
        out.extend_from_slice(&self.payload);
        out.extend_from_slice(&(self.refs.len() as u32).to_le_bytes());
        for addr in &self.refs {
            out.extend_from_slice(addr.as_bytes());
        }
        out
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, BlockDecodeError> {
        let mut pos = 0usize;

        let tag_len = read_u32(bytes, &mut pos)? as usize;
        let tag_bytes = read_bytes(bytes, &mut pos, tag_len)?;
        let tag_str =
            String::from_utf8(tag_bytes.to_vec()).map_err(|_| BlockDecodeError::InvalidUtf8)?;
        let tag = Symbol::intern(&tag_str);

        let payload_len = read_u32(bytes, &mut pos)? as usize;
        let payload = read_bytes(bytes, &mut pos, payload_len)?.to_vec();

        let refs_count = read_u32(bytes, &mut pos)? as usize;
        let mut refs = Vec::with_capacity(refs_count);
        for _ in 0..refs_count {
            let slice = read_bytes(bytes, &mut pos, ADDRESS_SIZE)?;
            let addr_bytes: [u8; ADDRESS_SIZE] =
                slice.try_into().expect("slice length matches address size");
            refs.push(Address::from(addr_bytes));
        }

        if pos != bytes.len() {
            return Err(BlockDecodeError::TrailingBytes);
        }

        Ok(Block { tag, payload, refs })
    }
}

fn read_u32(bytes: &[u8], pos: &mut usize) -> Result<u32, BlockDecodeError> {
    let slice = read_bytes(bytes, pos, 4)?;
    Ok(u32::from_le_bytes(
        slice.try_into().expect("slice length matches u32 size"),
    ))
}

fn read_bytes<'a>(
    bytes: &'a [u8],
    pos: &mut usize,
    len: usize,
) -> Result<&'a [u8], BlockDecodeError> {
    if *pos + len > bytes.len() {
        return Err(BlockDecodeError::Truncated);
    }
    let slice = &bytes[*pos..*pos + len];
    *pos += len;
    Ok(slice)
}
