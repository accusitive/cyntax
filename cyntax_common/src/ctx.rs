use codespan_reporting::files::{SimpleFile, SimpleFiles};
pub use string_interner;
use string_interner::{backend::StringBackend, symbol::SymbolU32};

pub type File = SimpleFile<String, String>;
/// The entire parsing context
/// This is used to store long term data that is needed between each step, string interning being the most obvious
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
    /// Resolve an interned string
    pub fn res(&self, sym: SymbolU32) -> &str {
        self.strings.resolve(sym).expect("failed to resolve interned string")
    }
    /// Interns a string
    pub fn int<S: AsRef<str>>(&mut self, s: S) -> SymbolU32 {
        self.strings.get_or_intern(s)
    }
    /// Interns a static string
    pub fn ints(&mut self, s: &'static str) -> SymbolU32 {
        self.strings.get_or_intern_static(s)
    }
    pub fn find_quoted_header(&self, name: &str) -> Option<String> {
        std::fs::read_to_string(name).ok()
    }
    pub fn find_bracketed_header(&self, name: &str) -> Option<String> {
        if let Ok(src) = std::fs::read_to_string(name) {
            return Some(src);
        } else if let Ok(src) = std::fs::read_to_string(format!("/usr/lib/gcc/x86_64-linux-gnu/13/include/{}", name)) {
            return Some(src);
        } else if let Ok(src) = std::fs::read_to_string(format!("/usr/local/include/{}", name)) {
            return Some(src);
        } else if let Ok(src) = std::fs::read_to_string(format!("/usr/include/x86_64-linux-gnu/{}", name)) {
            return Some(src);
        } else if let Ok(src) = std::fs::read_to_string(format!("/usr/include/{}", name)) {
            return Some(src);
        } else {
            None
        }
    }
}
pub trait HasContext {
    fn ctx(&self) -> &Context;
}
pub trait HasMutContext {
    fn ctx_mut(&mut self) -> &mut Context;
}

impl HasContext for Context {
    fn ctx(&self) -> &Context {
        self
    }
}
