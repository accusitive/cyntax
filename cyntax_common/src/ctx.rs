use codespan_reporting::files::{SimpleFile, SimpleFiles};
pub use string_interner;
use string_interner::backend::StringBackend;

pub type File = SimpleFile<String, String>;
#[derive(Debug)]
pub struct Context {
    pub files: SimpleFiles<String, String>,
    pub strings: string_interner::StringInterner<StringBackend>,
    pub current_file: usize,
}
impl Context {
    pub fn current_file(&self) -> &File {
        self.files.get(self.current_file).unwrap()
    }
}
pub trait HasContext {
    fn ctx(&self) -> &Context;
}
pub trait HasMutContext {
    fn ctx_mut(&mut self) -> &mut Context;
}
