pub use string_interner;
use std::collections::HashMap;
use string_interner::backend::{BucketBackend, BufferBackend, StringBackend};

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub source: String
}
#[derive(Debug)]
pub struct Context {
    pub files: HashMap<usize, File>,
    pub strings: string_interner::StringInterner<StringBackend>,
    pub current_file: usize
}
impl Context {
    pub fn current_file(&self) -> &File {
        self.files.get(&self.current_file).unwrap()
    }
}