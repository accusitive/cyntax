struct StrPieces<'a> {
    pieces: &'a [&'a str],
}
impl<'a> From<StrPieces<'a>> for String {
    fn from(value: StrPieces<'a>) -> Self {
        let mut s = String::new();
        for piece in value.pieces {
            s.push_str(*piece);
        }
        s
    }
}
fn main() {
    let string = "Hello something World!".to_string();
    let sp = StrPieces {
        pieces: &[&string[0..5], &string[5..6], &string[16..21]],
    };
    let stringified: String = sp.into();
    dbg!(&stringified);
}
