use fold_rs::core::{Address, Block, ADDRESS_SIZE};

#[test]
fn block_round_trip_simple() {
    let block = Block::new("greeting".to_string(), b"hello".to_vec(), Vec::new());
    let bytes = block.to_bytes();
    let decoded = Block::from_bytes(&bytes).expect("block should decode");

    assert_eq!(decoded.tag, "greeting");
    assert_eq!(decoded.payload, b"hello");
    assert_eq!(decoded, block);
}

#[test]
fn block_round_trip_with_refs() {
    let addr1 = Address::from([0xAA; ADDRESS_SIZE]);
    let addr2 = Address::from([0xBB; ADDRESS_SIZE]);
    let block = Block::new("node".to_string(), Vec::new(), vec![addr1, addr2]);

    let bytes = block.to_bytes();
    let decoded = Block::from_bytes(&bytes).expect("block should decode");

    assert_eq!(decoded.refs.len(), 2);
    assert_eq!(decoded, block);
}
