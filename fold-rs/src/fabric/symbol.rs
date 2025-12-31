use std::collections::HashMap;
use std::fmt;
use std::sync::{Mutex, OnceLock};

/// Global string interner for symbols.
/// Stores strings and returns indices for O(1) equality checks.
/// Uses a mutex-protected global to work correctly across threads.
static INTERNER: OnceLock<Mutex<Interner>> = OnceLock::new();

fn interner() -> &'static Mutex<Interner> {
    INTERNER.get_or_init(|| Mutex::new(Interner::new()))
}

struct Interner {
    strings: Vec<Box<str>>,
    map: HashMap<Box<str>, u32>,
}

impl Interner {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn intern(&mut self, s: &str) -> u32 {
        if let Some(&idx) = self.map.get(s) {
            return idx;
        }

        let idx = self.strings.len() as u32;
        let boxed: Box<str> = s.into();
        self.map.insert(boxed.clone(), idx);
        self.strings.push(boxed);
        idx
    }

    fn get(&self, idx: u32) -> &str {
        &self.strings[idx as usize]
    }
}

/// An interned symbol. Symbols with the same string content
/// have the same index, making equality checks O(1).
///
/// The Symbol is 4 bytes (u32 index) instead of 24+ bytes (String).
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u32);

impl Symbol {
    /// Intern a string and return a Symbol.
    /// If the string was already interned, returns the existing Symbol.
    pub fn intern(s: &str) -> Self {
        Symbol(interner().lock().unwrap().intern(s))
    }

    /// Get the string content of this symbol.
    pub fn as_str(&self) -> String {
        interner().lock().unwrap().get(self.0).to_string()
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({:?})", self.as_str())
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol::intern(s)
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Symbol::intern(&s)
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for Symbol {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == *other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_same_string() {
        let s1 = Symbol::intern("foo");
        let s2 = Symbol::intern("foo");
        assert_eq!(s1, s2);
        assert_eq!(s1.0, s2.0); // Same index
    }

    #[test]
    fn test_intern_different_strings() {
        let s1 = Symbol::intern("test_diff_1");
        let s2 = Symbol::intern("test_diff_2");
        assert_ne!(s1, s2);
        assert_ne!(s1.0, s2.0);
    }

    #[test]
    fn test_as_str() {
        let s = Symbol::intern("hello_test");
        assert_eq!(s.as_str(), "hello_test");
    }

    #[test]
    fn test_from_str() {
        let s: Symbol = "from_str_test".into();
        assert_eq!(s.as_str(), "from_str_test");
    }

    #[test]
    fn test_from_string() {
        let s: Symbol = String::from("from_string_test").into();
        assert_eq!(s.as_str(), "from_string_test");
    }

    #[test]
    fn test_eq_str() {
        let s = Symbol::intern("eq_test");
        assert!(s == "eq_test");
        assert!(s != "other");
    }

    #[test]
    fn test_display() {
        let s = Symbol::intern("display_test");
        assert_eq!(format!("{}", s), "display_test");
    }

    #[test]
    fn test_size() {
        assert_eq!(std::mem::size_of::<Symbol>(), 4);
    }
}
