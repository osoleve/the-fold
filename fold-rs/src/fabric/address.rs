pub const HASH_SIZE: usize = 32;
pub const ADDRESS_VERSION: u8 = 0;
pub const ADDRESS_SIZE: usize = 1 + HASH_SIZE;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Address(pub [u8; ADDRESS_SIZE]);

impl Address {
    pub fn new(version: u8, hash: [u8; HASH_SIZE]) -> Self {
        let mut bytes = [0u8; ADDRESS_SIZE];
        bytes[0] = version;
        bytes[1..].copy_from_slice(&hash);
        Address(bytes)
    }

    pub fn version(&self) -> u8 {
        self.0[0]
    }

    pub fn as_bytes(&self) -> &[u8; ADDRESS_SIZE] {
        &self.0
    }

    pub fn hash_bytes(&self) -> [u8; HASH_SIZE] {
        let mut hash = [0u8; HASH_SIZE];
        hash.copy_from_slice(&self.0[1..]);
        hash
    }
}

impl From<[u8; ADDRESS_SIZE]> for Address {
    fn from(bytes: [u8; ADDRESS_SIZE]) -> Self {
        Address(bytes)
    }
}
