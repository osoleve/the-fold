use fold_rs::fabric::{
    address_to_hex, hash_block, hex_to_address, Address, Block, Store, ADDRESS_SIZE,
    ADDRESS_VERSION,
};

#[test]
fn cas_store_and_fetch() {
    let mut store = Store::new();
    let block = Block::new("test".to_string(), b"hello".to_vec(), Vec::new());

    let addr = store.store(block.clone());
    assert_eq!(addr.version(), ADDRESS_VERSION);
    assert_eq!(addr.as_bytes().len(), ADDRESS_SIZE);

    let fetched = store.fetch(&addr).expect("block should be stored");
    assert_eq!(fetched, &block);
}

#[test]
fn cas_hash_stability() {
    let block = Block::new("test".to_string(), b"hello".to_vec(), Vec::new());
    let addr1 = hash_block(&block);
    let addr2 = hash_block(&block);

    assert_eq!(addr1, addr2);

    let other = Block::new("test".to_string(), b"world".to_vec(), Vec::new());
    let addr3 = hash_block(&other);
    assert_ne!(addr1, addr3);
}

#[test]
fn cas_hex_round_trip() {
    let address = Address::from([0x11; ADDRESS_SIZE]);
    let hex = address_to_hex(&address);
    let decoded = hex_to_address(&hex).expect("valid hex");

    assert_eq!(decoded, address);
}
