//! Store traversal and indexing for The Fold's CAS.
//!
//! The store is organized as:
//!   .store/objects/{xx}/{hash}  - Block files (xx = first 2 hex chars)
//!   .store/pins/{hash}.pin      - Pin markers
//!   .store/heads/{name}.head    - Named references

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use crate::block::{Block, ADDRESS_SIZE};

/// Error type for store operations.
#[derive(Debug)]
pub enum StoreError {
    Io(std::io::Error),
    Parse(crate::block::ParseError),
    NotFound(String),
}

impl std::fmt::Display for StoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "I/O error: {e}"),
            Self::Parse(e) => write!(f, "parse error: {e}"),
            Self::NotFound(h) => write!(f, "block not found: {h}"),
        }
    }
}

impl std::error::Error for StoreError {}

impl From<std::io::Error> for StoreError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<crate::block::ParseError> for StoreError {
    fn from(e: crate::block::ParseError) -> Self {
        Self::Parse(e)
    }
}

/// Block metadata stored in memory (payload kept on disk).
#[derive(Debug, Clone)]
pub struct BlockMeta {
    pub hash: String,
    pub tag: String,
    pub payload_size: usize,
    pub refs_count: usize,
    pub pinned: bool,
}

/// A head (named reference).
#[derive(Debug, Clone)]
pub struct Head {
    pub name: String,
    pub hash: String,
}

/// The content-addressed store.
pub struct Store {
    root: PathBuf,
    /// Block metadata index (hash -> metadata).
    blocks: RwLock<HashMap<String, BlockMeta>>,
    /// Tag index (tag -> list of hashes).
    tag_index: RwLock<HashMap<String, Vec<String>>>,
    /// Inbound reference counts (hash -> count of blocks referencing it).
    ref_counts: RwLock<HashMap<String, usize>>,
    /// Hash to shard directory mapping for efficient lookup.
    hash_shards: RwLock<HashMap<String, String>>,
}

impl Store {
    /// Open a store at the given path and build indexes.
    pub fn open(root: impl AsRef<Path>) -> Result<Self, StoreError> {
        let root = root.as_ref().to_path_buf();
        let store = Self {
            root,
            blocks: RwLock::new(HashMap::new()),
            tag_index: RwLock::new(HashMap::new()),
            ref_counts: RwLock::new(HashMap::new()),
            hash_shards: RwLock::new(HashMap::new()),
        };
        store.build_indexes()?;
        Ok(store)
    }

    /// Build in-memory indexes from the store on disk.
    fn build_indexes(&self) -> Result<(), StoreError> {
        let objects_dir = self.root.join("objects");
        if !objects_dir.exists() {
            return Ok(()); // Empty store
        }

        let mut blocks = self.blocks.write().map_err(|_| {
            StoreError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                "lock poisoned",
            ))
        })?;
        let mut tag_index = self.tag_index.write().map_err(|_| {
            StoreError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                "lock poisoned",
            ))
        })?;
        let mut ref_counts = self.ref_counts.write().map_err(|_| {
            StoreError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                "lock poisoned",
            ))
        })?;
        let mut hash_shards = self.hash_shards.write().map_err(|_| {
            StoreError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                "lock poisoned",
            ))
        })?;

        // Iterate over prefix directories (00-ff)
        for prefix_entry in fs::read_dir(&objects_dir)? {
            let prefix_entry = prefix_entry?;
            let prefix_path = prefix_entry.path();
            if !prefix_path.is_dir() {
                continue;
            }
            let prefix = prefix_path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("")
                .to_string();

            // Iterate over block files in this prefix
            for file_entry in fs::read_dir(&prefix_path)? {
                let file_entry = file_entry?;
                let file_path = file_entry.path();
                if !file_path.is_file() {
                    continue;
                }
                let suffix = file_path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("")
                    .to_string();

                // The filename is the block hash
                // Can be 64 chars (legacy 32-byte hash) or 66 chars (33-byte address)
                let hash = suffix.clone();

                // Accept both legacy (64 chars) and versioned (66 chars) hashes
                if hash.len() != 64 && hash.len() != ADDRESS_SIZE * 2 {
                    continue;
                }

                // Parse block to get metadata
                match fs::read(&file_path) {
                    Ok(data) => match Block::from_bytes(&data) {
                        Ok(block) => {
                            let pinned = self.is_pinned(&hash);

                            // Store metadata
                            let meta = BlockMeta {
                                hash: hash.clone(),
                                tag: block.tag.clone(),
                                payload_size: block.payload.len(),
                                refs_count: block.refs.len(),
                                pinned,
                            };
                            blocks.insert(hash.clone(), meta);

                            // Track which shard directory contains this hash
                            hash_shards.insert(hash.clone(), prefix.clone());

                            // Index by tag
                            tag_index.entry(block.tag).or_default().push(hash.clone());

                            // Count inbound refs
                            for r in &block.refs {
                                let ref_hash = r.to_hex();
                                *ref_counts.entry(ref_hash).or_default() += 1;
                            }
                        }
                        Err(_) => {
                            // Skip unparseable blocks
                            continue;
                        }
                    },
                    Err(_) => continue,
                }
            }
        }

        Ok(())
    }

    /// Check if a block is pinned.
    pub fn is_pinned(&self, hash: &str) -> bool {
        self.root
            .join("pins")
            .join(format!("{}.pin", hash))
            .exists()
    }

    /// Get the number of indexed blocks.
    pub fn block_count(&self) -> usize {
        self.blocks.read().map(|b| b.len()).unwrap_or(0)
    }

    /// Get all unique tags and their counts.
    pub fn tag_counts(&self) -> HashMap<String, usize> {
        self.tag_index
            .read()
            .map(|idx| idx.iter().map(|(k, v)| (k.clone(), v.len())).collect())
            .unwrap_or_default()
    }

    /// List blocks with pagination and optional tag filter.
    pub fn list_blocks(
        &self,
        offset: usize,
        limit: usize,
        tag_filter: Option<&str>,
        prefix_filter: Option<&str>,
    ) -> Vec<BlockMeta> {
        let blocks = match self.blocks.read() {
            Ok(b) => b,
            Err(_) => return Vec::new(),
        };

        let mut results: Vec<&BlockMeta> = blocks
            .values()
            .filter(|m| {
                if let Some(tag) = tag_filter {
                    if m.tag != tag {
                        return false;
                    }
                }
                if let Some(prefix) = prefix_filter {
                    if !m.hash.starts_with(prefix) {
                        return false;
                    }
                }
                true
            })
            .collect();

        // Sort by hash for consistent ordering
        results.sort_by(|a, b| a.hash.cmp(&b.hash));

        results
            .into_iter()
            .skip(offset)
            .take(limit)
            .cloned()
            .collect()
    }

    /// Get a block's metadata by hash.
    pub fn get_meta(&self, hash: &str) -> Option<BlockMeta> {
        self.blocks.read().ok()?.get(hash).cloned()
    }

    /// Load and parse a full block from disk.
    pub fn load_block(&self, hash: &str) -> Result<Block, StoreError> {
        // Validate hash format (hex chars only)
        if !hash.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(StoreError::NotFound(hash.to_string()));
        }
        // Accept both legacy (64 chars) and versioned (66 chars) hashes
        if hash.len() != 64 && hash.len() != ADDRESS_SIZE * 2 {
            return Err(StoreError::NotFound(hash.to_string()));
        }

        let objects_dir = self.root.join("objects");

        // First, check the indexed shard location
        if let Ok(shards) = self.hash_shards.read() {
            if let Some(shard) = shards.get(hash) {
                let path = objects_dir.join(shard).join(hash);
                if path.exists() {
                    let data = fs::read(&path)?;
                    return Block::from_bytes(&data).map_err(StoreError::Parse);
                }
            }
        }

        // Fall back to expected prefix directory
        let expected_prefix = &hash[..2];
        let expected_path = objects_dir.join(expected_prefix).join(hash);
        if expected_path.exists() {
            let data = fs::read(&expected_path)?;
            return Block::from_bytes(&data).map_err(StoreError::Parse);
        }

        // If still not found, scan all directories (handles unindexed blocks)
        for entry in fs::read_dir(&objects_dir)? {
            let entry = entry?;
            let shard_path = entry.path();
            if !shard_path.is_dir() {
                continue;
            }
            let path = shard_path.join(hash);
            if path.exists() {
                let data = fs::read(&path)?;
                return Block::from_bytes(&data).map_err(StoreError::Parse);
            }
        }

        Err(StoreError::NotFound(hash.to_string()))
    }

    /// Get raw payload bytes for a block.
    pub fn get_payload(&self, hash: &str) -> Result<Vec<u8>, StoreError> {
        let block = self.load_block(hash)?;
        Ok(block.payload)
    }

    /// Get orphan blocks (blocks with no inbound references).
    pub fn get_orphans(&self, limit: usize) -> Vec<BlockMeta> {
        let blocks = match self.blocks.read() {
            Ok(b) => b,
            Err(_) => return Vec::new(),
        };
        let ref_counts = match self.ref_counts.read() {
            Ok(r) => r,
            Err(_) => return Vec::new(),
        };

        let mut orphans: Vec<&BlockMeta> = blocks
            .values()
            .filter(|m| !ref_counts.contains_key(&m.hash))
            .collect();

        orphans.sort_by(|a, b| a.hash.cmp(&b.hash));
        orphans.into_iter().take(limit).cloned().collect()
    }

    /// Get most-referenced blocks (popular).
    pub fn get_popular(&self, limit: usize) -> Vec<(BlockMeta, usize)> {
        let blocks = match self.blocks.read() {
            Ok(b) => b,
            Err(_) => return Vec::new(),
        };
        let ref_counts = match self.ref_counts.read() {
            Ok(r) => r,
            Err(_) => return Vec::new(),
        };

        let mut popular: Vec<(&String, &usize)> = ref_counts.iter().collect();
        popular.sort_by(|a, b| b.1.cmp(a.1));

        popular
            .into_iter()
            .take(limit)
            .filter_map(|(hash, count)| blocks.get(hash).map(|m| (m.clone(), *count)))
            .collect()
    }

    /// Get a subgraph rooted at a hash (for visualization).
    pub fn get_subgraph(
        &self,
        root_hash: &str,
        max_depth: usize,
    ) -> Result<(Vec<BlockMeta>, Vec<(String, String)>), StoreError> {
        let mut nodes: Vec<BlockMeta> = Vec::new();
        let mut edges: Vec<(String, String)> = Vec::new();
        let mut visited: HashMap<String, bool> = HashMap::new();
        let mut queue: Vec<(String, usize)> = vec![(root_hash.to_string(), 0)];

        while let Some((hash, depth)) = queue.pop() {
            if visited.contains_key(&hash) || depth > max_depth {
                continue;
            }
            visited.insert(hash.clone(), true);

            if let Some(meta) = self.get_meta(&hash) {
                nodes.push(meta);
            }

            // Load block to get refs
            if let Ok(block) = self.load_block(&hash) {
                for r in &block.refs {
                    let ref_hash = r.to_hex();
                    edges.push((hash.clone(), ref_hash.clone()));
                    if !visited.contains_key(&ref_hash) {
                        queue.push((ref_hash, depth + 1));
                    }
                }
            }
        }

        Ok((nodes, edges))
    }

    /// Search blocks by tag or payload content.
    pub fn search(&self, query: &str, tag_filter: Option<&str>, limit: usize) -> Vec<BlockMeta> {
        let blocks = match self.blocks.read() {
            Ok(b) => b,
            Err(_) => return Vec::new(),
        };

        let query_lower = query.to_lowercase();
        let mut results: Vec<BlockMeta> = Vec::new();

        for meta in blocks.values() {
            // Filter by tag if specified
            if let Some(tag) = tag_filter {
                if meta.tag != tag {
                    continue;
                }
            }

            // Check if hash starts with query
            if meta.hash.starts_with(query) {
                results.push(meta.clone());
                continue;
            }

            // Check if tag contains query
            if meta.tag.to_lowercase().contains(&query_lower) {
                results.push(meta.clone());
                continue;
            }

            // For deeper search, load payload
            if let Ok(block) = self.load_block(&meta.hash) {
                if let Some(payload_str) = block.payload_as_string() {
                    if payload_str.to_lowercase().contains(&query_lower) {
                        results.push(meta.clone());
                    }
                }
            }

            if results.len() >= limit {
                break;
            }
        }

        results.truncate(limit);
        results
    }

    /// List all heads (named references).
    pub fn list_heads(&self) -> Result<Vec<Head>, StoreError> {
        let heads_dir = self.root.join("heads");
        if !heads_dir.exists() {
            return Ok(Vec::new());
        }

        let mut heads = Vec::new();
        self.collect_heads(&heads_dir, "", &mut heads)?;
        Ok(heads)
    }

    /// Recursively collect heads from a directory.
    fn collect_heads(
        &self,
        dir: &Path,
        prefix: &str,
        heads: &mut Vec<Head>,
    ) -> Result<(), StoreError> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let name = entry.file_name().to_str().unwrap_or("").to_string();

            if path.is_dir() {
                // Recurse into subdirectory
                let new_prefix = if prefix.is_empty() {
                    name
                } else {
                    format!("{}/{}", prefix, name)
                };
                self.collect_heads(&path, &new_prefix, heads)?;
            } else if name.ends_with(".head") {
                // Read head file
                let head_name = if prefix.is_empty() {
                    name.trim_end_matches(".head").to_string()
                } else {
                    format!("{}/{}", prefix, name.trim_end_matches(".head"))
                };
                if let Ok(content) = fs::read_to_string(&path) {
                    let hash = content.trim().to_string();
                    heads.push(Head {
                        name: head_name,
                        hash,
                    });
                }
            }
        }
        Ok(())
    }

    /// Get a specific head by name.
    pub fn get_head(&self, name: &str) -> Result<Head, StoreError> {
        let path = self.root.join("heads").join(format!("{}.head", name));
        let content = fs::read_to_string(&path)?;
        let hash = content.trim().to_string();
        Ok(Head {
            name: name.to_string(),
            hash,
        })
    }
}
