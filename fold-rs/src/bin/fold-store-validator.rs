// Block Store Validator - Verifies .store/ integrity
//
// Checks:
// 1. Hash integrity: Block content matches its address
// 2. Reference integrity: All refs point to existing blocks
// 3. Decode integrity: All blocks can be decoded
// 4. Orphan detection: Blocks not reachable from pins/heads
// 5. Pin/head consistency: Referenced blocks exist

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use fold_rs::fabric::address::{ADDRESS_VERSION, Address, HASH_SIZE};
use fold_rs::fabric::block::Block;
use fold_rs::fabric::cas::{address_to_hex, hash_block};
use fold_rs::fabric::hex::hex_to_bytes;

/// Helper to convert a hash-only hex string (64 chars) to an Address
fn hash_hex_to_address(hex: &str) -> Result<Address, String> {
    let hash_bytes = hex_to_bytes(hex).map_err(|e| format!("{:?}", e))?;
    if hash_bytes.len() != HASH_SIZE {
        return Err(format!(
            "Invalid hash length: expected {}, got {}",
            HASH_SIZE,
            hash_bytes.len()
        ));
    }
    let mut hash_array = [0u8; HASH_SIZE];
    hash_array.copy_from_slice(&hash_bytes);
    Ok(Address::new(ADDRESS_VERSION, hash_array))
}

#[derive(Debug, Default)]
struct ValidationReport {
    total_blocks: usize,
    hash_mismatches: Vec<String>,
    decode_errors: Vec<String>,
    missing_refs: Vec<(String, String)>, // (block_addr, missing_ref)
    orphaned_blocks: Vec<String>,
    missing_pins: Vec<String>,
    missing_heads: Vec<String>,
}

impl ValidationReport {
    fn is_valid(&self) -> bool {
        self.hash_mismatches.is_empty()
            && self.decode_errors.is_empty()
            && self.missing_refs.is_empty()
            && self.missing_pins.is_empty()
            && self.missing_heads.is_empty()
    }

    fn print(&self) {
        println!("Block Store Validation Report");
        println!("==============================\n");
        println!("Total blocks: {}", self.total_blocks);

        if self.is_valid() {
            println!("\n✓ All checks passed!");
            if !self.orphaned_blocks.is_empty() {
                println!(
                    "\nNote: {} orphaned block(s) found (not reachable from pins/heads)",
                    self.orphaned_blocks.len()
                );
            }
        } else {
            println!("\n❌ Validation FAILED!\n");

            if !self.hash_mismatches.is_empty() {
                println!("Hash mismatches ({}):", self.hash_mismatches.len());
                for addr in &self.hash_mismatches {
                    println!("  - {}", addr);
                }
                println!();
            }

            if !self.decode_errors.is_empty() {
                println!("Decode errors ({}):", self.decode_errors.len());
                for addr in &self.decode_errors {
                    println!("  - {}", addr);
                }
                println!();
            }

            if !self.missing_refs.is_empty() {
                println!("Missing references ({}):", self.missing_refs.len());
                for (block, ref_addr) in &self.missing_refs {
                    println!("  - {} → {} (missing)", block, ref_addr);
                }
                println!();
            }

            if !self.missing_pins.is_empty() {
                println!("Missing pinned blocks ({}):", self.missing_pins.len());
                for addr in &self.missing_pins {
                    println!("  - {}", addr);
                }
                println!();
            }

            if !self.missing_heads.is_empty() {
                println!("Missing head blocks ({}):", self.missing_heads.len());
                for addr in &self.missing_heads {
                    println!("  - {}", addr);
                }
                println!();
            }
        }

        if !self.orphaned_blocks.is_empty() {
            println!("Orphaned blocks ({}):", self.orphaned_blocks.len());
            for addr in self.orphaned_blocks.iter().take(10) {
                println!("  - {}", addr);
            }
            if self.orphaned_blocks.len() > 10 {
                println!("  ... and {} more", self.orphaned_blocks.len() - 10);
            }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let store_path = if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        PathBuf::from(".store")
    };

    if !store_path.exists() {
        eprintln!("Error: Store directory not found: {:?}", store_path);
        eprintln!("Usage: {} [store_path]", args[0]);
        std::process::exit(1);
    }

    println!("Validating block store: {:?}\n", store_path);

    let report = validate_store(&store_path);
    report.print();

    if !report.is_valid() {
        std::process::exit(1);
    }
}

fn validate_store(store_path: &Path) -> ValidationReport {
    let mut report = ValidationReport::default();

    // Step 1: Load all blocks and validate hash integrity
    // Objects are stored git-style: .store/objects/ab/ab123...
    let objects_dir = store_path.join("objects");
    let mut blocks: HashMap<Address, Block> = HashMap::new();

    if let Ok(shard_dirs) = fs::read_dir(&objects_dir) {
        for shard_entry in shard_dirs.flatten() {
            let shard_path = shard_entry.path();
            if shard_path.is_dir()
                && let Ok(entries) = fs::read_dir(&shard_path)
            {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_file()
                        && let Some(filename) = path.file_name().and_then(|n| n.to_str())
                    {
                        report.total_blocks += 1;

                        // Validate filename is valid hex hash (64 chars = 32 bytes)
                        // Filesystem stores just the hash, not the full address with version
                        let addr = match hash_hex_to_address(filename) {
                            Ok(a) => a,
                            Err(e) => {
                                report.decode_errors.push(format!("{} ({})", filename, e));
                                continue;
                            }
                        };

                        // Read and decode block
                        let block_bytes = match fs::read(&path) {
                            Ok(b) => b,
                            Err(e) => {
                                report
                                    .decode_errors
                                    .push(format!("{} (read error: {})", filename, e));
                                continue;
                            }
                        };

                        let block = match Block::from_bytes(&block_bytes) {
                            Ok(b) => b,
                            Err(e) => {
                                report
                                    .decode_errors
                                    .push(format!("{} (decode error: {})", filename, e));
                                continue;
                            }
                        };

                        // Verify hash matches filename
                        let computed_addr = hash_block(&block);
                        if computed_addr != addr {
                            report.hash_mismatches.push(format!(
                                "{} (expected {}, got {})",
                                filename,
                                address_to_hex(&addr),
                                address_to_hex(&computed_addr)
                            ));
                            continue;
                        }

                        blocks.insert(addr, block);
                    }
                }
            }
        }
    }

    // Step 2: Validate all refs point to existing blocks
    for (addr, block) in &blocks {
        for ref_addr in &block.refs {
            if !blocks.contains_key(ref_addr) {
                report
                    .missing_refs
                    .push((address_to_hex(addr), address_to_hex(ref_addr)));
            }
        }
    }

    // Step 3: Validate pins
    let pins_dir = store_path.join("pins");
    if let Ok(entries) = fs::read_dir(&pins_dir) {
        for entry in entries.flatten() {
            if let Some(filename) = entry.file_name().to_str()
                && let Ok(addr) = hash_hex_to_address(filename)
                && !blocks.contains_key(&addr)
            {
                report.missing_pins.push(filename.to_string());
            }
        }
    }

    // Step 4: Validate heads
    let heads_dir = store_path.join("heads");
    if let Ok(entries) = fs::read_dir(&heads_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file()
                && let Ok(content) = fs::read_to_string(&path)
            {
                let addr_hex = content.trim();
                if let Ok(addr) = hash_hex_to_address(addr_hex)
                    && !blocks.contains_key(&addr)
                {
                    report.missing_heads.push(addr_hex.to_string());
                }
            }
        }
    }

    // Step 5: Find orphaned blocks (not reachable from pins/heads)
    let mut reachable = HashSet::new();
    let mut to_visit = Vec::new();

    // Start from pins
    if let Ok(entries) = fs::read_dir(&pins_dir) {
        for entry in entries.flatten() {
            if let Some(filename) = entry.file_name().to_str()
                && let Ok(addr) = hash_hex_to_address(filename)
                && blocks.contains_key(&addr)
            {
                to_visit.push(addr);
            }
        }
    }

    // Start from heads
    if let Ok(entries) = fs::read_dir(&heads_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file()
                && let Ok(content) = fs::read_to_string(&path)
            {
                let addr_hex = content.trim();
                if let Ok(addr) = hash_hex_to_address(addr_hex)
                    && blocks.contains_key(&addr)
                {
                    to_visit.push(addr);
                }
            }
        }
    }

    // BFS to find all reachable blocks
    while let Some(addr) = to_visit.pop() {
        if reachable.insert(addr)
            && let Some(block) = blocks.get(&addr)
        {
            for ref_addr in &block.refs {
                if blocks.contains_key(ref_addr) {
                    to_visit.push(*ref_addr);
                }
            }
        }
    }

    // Find orphans
    for addr in blocks.keys() {
        if !reachable.contains(addr) {
            report.orphaned_blocks.push(address_to_hex(addr));
        }
    }

    report
}
