use std::collections::{HashMap, HashSet, VecDeque};

use crate::fabric::{
    address::{ADDRESS_SIZE, ADDRESS_VERSION, Address},
    block::Block,
    hex::{HexError, bytes_to_hex, hex_to_bytes},
    sha256::sha256,
};

pub fn hash_block(block: &Block) -> Address {
    let hash = sha256(&block.to_bytes());
    Address::new(ADDRESS_VERSION, hash)
}

pub fn address_to_hex(address: &Address) -> String {
    bytes_to_hex(address.as_bytes())
}

pub fn hex_to_address(hex: &str) -> Result<Address, HexError> {
    let bytes = hex_to_bytes(hex)?;
    if bytes.len() != ADDRESS_SIZE {
        return Err(HexError::InvalidLength {
            expected: ADDRESS_SIZE,
            got: bytes.len(),
        });
    }
    let mut addr = [0u8; ADDRESS_SIZE];
    addr.copy_from_slice(&bytes);
    Ok(Address::from(addr))
}

#[derive(Debug, Default)]
pub struct Store {
    blocks: HashMap<Address, Block>,
    pinned: HashSet<Address>,
}

impl Store {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn store(&mut self, block: Block) -> Address {
        let address = hash_block(&block);
        self.blocks.insert(address, block);
        address
    }

    pub fn fetch(&self, address: &Address) -> Option<&Block> {
        self.blocks.get(address)
    }

    pub fn stored(&self, address: &Address) -> bool {
        self.blocks.contains_key(address)
    }

    pub fn pin(&mut self, address: Address) {
        self.pinned.insert(address);
    }

    pub fn pinned(&self, address: &Address) -> bool {
        self.pinned.contains(address)
    }

    pub fn unpin(&mut self, address: &Address) {
        self.pinned.remove(address);
    }

    pub fn store_count(&self) -> usize {
        self.blocks.len()
    }

    pub fn store_hashes(&self) -> Vec<Address> {
        self.blocks.keys().copied().collect()
    }

    pub fn collect_refs(&self, root: &Address) -> Vec<Address> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        let mut results = Vec::new();

        queue.push_back(*root);
        while let Some(current) = queue.pop_front() {
            if !visited.insert(current) {
                continue;
            }
            results.push(current);
            if let Some(block) = self.blocks.get(&current) {
                for addr in &block.refs {
                    if !visited.contains(addr) {
                        queue.push_back(*addr);
                    }
                }
            }
        }

        results
    }

    pub fn pin_tree(&mut self, root: &Address) -> usize {
        let mut count = 0;
        for addr in self.collect_refs(root) {
            if self.pinned.insert(addr) {
                count += 1;
            }
        }
        count
    }

    pub fn unpin_tree(&mut self, root: &Address) -> usize {
        let mut count = 0;
        for addr in self.collect_refs(root) {
            if self.pinned.remove(&addr) {
                count += 1;
            }
        }
        count
    }

    pub fn gc(&mut self) -> (usize, usize) {
        let before = self.blocks.len();
        self.blocks.retain(|addr, _| self.pinned.contains(addr));
        let after = self.blocks.len();
        (before - after, after)
    }

    pub fn gc_with_roots(&mut self, roots: &[Address]) -> (usize, usize) {
        let saved_pins = self.pinned.clone();
        self.pinned.clear();

        for root in roots {
            if self.blocks.contains_key(root) {
                self.pin_tree(root);
            }
        }

        let (collected, remaining) = self.gc();

        for addr in self.blocks.keys() {
            if saved_pins.contains(addr) {
                self.pinned.insert(*addr);
            }
        }

        (collected, remaining)
    }
}
