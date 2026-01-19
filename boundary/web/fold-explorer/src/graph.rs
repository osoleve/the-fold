//! Graph analysis utilities for the CAS explorer.

use crate::json::Json;
use crate::store::{BlockMeta, Store};

/// Store statistics.
pub struct StoreStats {
    pub total_blocks: usize,
    pub total_pinned: usize,
    pub tags: Vec<(String, usize)>,
}

impl StoreStats {
    /// Gather statistics from a store.
    pub fn from_store(store: &Store) -> Self {
        let total_blocks = store.block_count();
        let tag_counts = store.tag_counts();

        let mut tags: Vec<(String, usize)> = tag_counts.into_iter().collect();
        tags.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending

        // Count pinned (estimate from first 1000 blocks)
        let blocks = store.list_blocks(0, 1000, None, None);
        let total_pinned = blocks.iter().filter(|b| b.pinned).count();

        Self {
            total_blocks,
            total_pinned,
            tags,
        }
    }

    /// Convert to JSON.
    pub fn to_json(&self) -> String {
        let tags_json: Vec<String> = self
            .tags
            .iter()
            .map(|(tag, count)| {
                Json::object(&[("tag", Json::string(tag)), ("count", Json::number(*count))])
            })
            .collect();

        Json::object(&[
            ("total_blocks", Json::number(self.total_blocks)),
            ("total_pinned", Json::number(self.total_pinned)),
            ("tags", Json::array(&tags_json)),
        ])
    }
}

/// Subgraph data for visualization.
pub struct Subgraph {
    pub nodes: Vec<BlockMeta>,
    pub edges: Vec<(String, String)>,
}

impl Subgraph {
    /// Convert to D3.js-compatible JSON.
    pub fn to_json(&self) -> String {
        let nodes_json: Vec<String> = self
            .nodes
            .iter()
            .map(|m| {
                Json::object(&[
                    ("id", Json::string(&m.hash)),
                    ("tag", Json::string(&m.tag)),
                    ("size", Json::number(m.payload_size)),
                    ("refs", Json::number(m.refs_count)),
                    ("pinned", Json::bool(m.pinned)),
                ])
            })
            .collect();

        let edges_json: Vec<String> = self
            .edges
            .iter()
            .map(|(source, target)| {
                Json::object(&[
                    ("source", Json::string(source)),
                    ("target", Json::string(target)),
                ])
            })
            .collect();

        Json::object(&[
            ("nodes", Json::array(&nodes_json)),
            ("edges", Json::array(&edges_json)),
        ])
    }
}

/// Convert a BlockMeta to JSON.
pub fn block_meta_to_json(meta: &BlockMeta) -> String {
    Json::object(&[
        ("hash", Json::string(&meta.hash)),
        ("tag", Json::string(&meta.tag)),
        ("payload_size", Json::number(meta.payload_size)),
        ("refs_count", Json::number(meta.refs_count)),
        ("pinned", Json::bool(meta.pinned)),
    ])
}

/// Convert a list of BlockMeta to JSON array.
pub fn block_list_to_json(blocks: &[BlockMeta]) -> String {
    let items: Vec<String> = blocks.iter().map(block_meta_to_json).collect();
    Json::array(&items)
}

/// Convert popular blocks (with ref counts) to JSON.
pub fn popular_to_json(popular: &[(BlockMeta, usize)]) -> String {
    let items: Vec<String> = popular
        .iter()
        .map(|(meta, count)| {
            Json::object(&[
                ("hash", Json::string(&meta.hash)),
                ("tag", Json::string(&meta.tag)),
                ("payload_size", Json::number(meta.payload_size)),
                ("refs_count", Json::number(meta.refs_count)),
                ("pinned", Json::bool(meta.pinned)),
                ("inbound_refs", Json::number(*count)),
            ])
        })
        .collect();
    Json::array(&items)
}
