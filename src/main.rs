use std::{ops::Range, str::Chars};

use peekmore::{PeekMore, PeekMoreIterator};
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
// fn main() {
//     let string = "Hello something World!".to_string();
//     let sp = StrPieces {
//         pieces: &[&string[0..5], &string[5..6], &string[16..21]],
//     };
//     let stringified: String = sp.into();
//     dbg!(&stringified);
// }

impl<'a> PartialEq for StrPieces<'a> {
    fn eq(&self, other: &Self) -> bool {
        let self_iter = self.pieces.iter().flat_map(|s| s.chars());
        let other_iter = other.pieces.iter().flat_map(|s| s.chars());
        self_iter.eq(other_iter) // Compare character by character
    }
}

pub struct CLexerIterator<'a> {
    chars: PeekMoreIterator<Chars<'a>>,
    current_pos: usize,
}
#[derive(Debug)]
pub enum Tok {
    Char(char),
    Whitespace,
    Error,
}
impl<'a> Iterator for CLexerIterator<'a> {
    type Item = (Range<usize>, Tok);

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;

        let token = if c == '\\' && self.chars.peek_nth(0).map(|c| *c) == Some('\n') {
            self.next().unwrap();
            self.current_pos += 1;
            self.next()
        } else {
            Some((
                self.current_pos..self.current_pos + 1,
                match c {
                    ' ' | '\t' | '\n' => Tok::Whitespace,
                    _ => Tok::Error,
                },
            ))
            // Some((self.current_pos..self.current_pos + 1, Tok::Char(c)))
        };
        self.current_pos = self.current_pos + 1;
        token
    }
}

fn main() {
    let source = "int main\\\na";
    let mut i = CLexerIterator {
        current_pos: 0,
        chars: source.chars().peekmore(),
    };
    let tokens = i.collect::<Vec<_>>();
    dbg!(&tokens);
    // while let Some((span, ch)) = i.next() {
    //     // println!("{:#?}", token);
    //     assert_eq!(&source[span], format!("{}", ch).as_str());
    // }
}
