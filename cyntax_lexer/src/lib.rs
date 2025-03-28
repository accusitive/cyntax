pub mod prelexer;
pub mod lexer;
#[derive(Debug)]
pub struct StrPieces<'a> {
    pieces: Vec<&'a str>,
}
impl<'a> From<StrPieces<'a>> for String {
    fn from(value: StrPieces<'a>) -> Self {
        let mut s = String::new();
        for piece in value.pieces {
            s.push_str(piece);
        }
        s
    }
}
impl<'a> PartialEq for StrPieces<'a> {
    fn eq(&self, other: &Self) -> bool {
        let self_iter = self.pieces.iter().flat_map(|s| s.chars());
        let other_iter = other.pieces.iter().flat_map(|s| s.chars());
        self_iter.eq(other_iter) // Compare character by character
    }
}
