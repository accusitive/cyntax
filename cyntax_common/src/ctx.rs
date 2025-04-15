use codespan_reporting::files::{Files, SimpleFile, SimpleFiles};
use std::collections::HashMap;
pub use string_interner;
use string_interner::backend::{BucketBackend, BufferBackend, StringBackend};

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
// impl<'a> Files<'a> for Context {
//     type FileId = usize;

//     type Name = &'a String;

//     type Source = &'a String;

//     fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
//         match self.files.get(&id) {
//             Some(file) => Ok(&file.name),
//             None => Err(codespan_reporting::files::Error::FileMissing),
//         }
//     }

//     fn source(&'a self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
//         match self.files.get(&id) {
//             Some(file) => Ok(&file.source),
//             None => Err(codespan_reporting::files::Error::FileMissing),
//         }
//     }

//     fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, codespan_reporting::files::Error> {
//         let ls =codespan_reporting::files::line_starts(self.source(id)?);
//         let ln = ls.collect::<Vec<_>>().binary_search(&byte_index).unwrap_or_else(|next_line| next_line-1);
//         Ok(ln)
//     }

//     fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
//         SimpleFiles
//         todo!()
//     }
// }
