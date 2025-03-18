use crate::lexer::{HeaderNameKind, PreprocessingToken};

use super::{Preprocessor, L};

impl Preprocessor {
    pub fn locate_header(&mut self, header_name: &L<PreprocessingToken>) -> Option<String> {
        let (header_name, kind) = header_name.as_header_name().unwrap();
        let mut header_content = None;
        let paths = ["./", "/usr/include/"];
        for path in paths {
            if let Ok(content) = std::fs::read_to_string(format!("{}{}", path, header_name)) {
                header_content = Some(content);
                break;
            }
        }
        header_content
    }
}
