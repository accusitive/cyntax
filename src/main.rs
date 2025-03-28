use std::{iter::Peekable, ops::Range, str::Chars};

use peekmore::{PeekMore, PeekMoreIterator};

#[cfg(test)]
mod tests;

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
impl<'a> PartialEq for StrPieces<'a> {
    fn eq(&self, other: &Self) -> bool {
        let self_iter = self.pieces.iter().flat_map(|s| s.chars());
        let other_iter = other.pieces.iter().flat_map(|s| s.chars());
        self_iter.eq(other_iter) // Compare character by character
    }
}

pub struct SkipEscapedNewlinesIter<'a> {
    chars: PeekMoreIterator<Chars<'a>>,
    current_pos: usize,
}
impl<'a> Iterator for SkipEscapedNewlinesIter<'a> {
    type Item = (Range<usize>, char);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.current_pos;

        let mut current_character = self.chars.next()?;
        let mut length = current_character.len_utf8();

        // Handle trigraphs
        if current_character == '?' && self.chars.peek() == Some(&'?') {
            let trigaph_replacement = match self.chars.peek_nth(1) {
                Some('=') => Some('#'),
                Some('/') => Some('\\'),
                Some('\'') => Some('^'),
                Some('(') => Some('['),
                Some(')') => Some(']'),
                Some('!') => Some('|'),
                Some('<') => Some('{'),
                Some('>') => Some('}'),
                Some('-') => Some('~'),

                Some(_) | None => None,
            };
            if let Some(replacement) = trigaph_replacement {
                length += self.chars.next().unwrap().len_utf8(); // Second ?
                length += self.chars.next().unwrap().len_utf8(); // Actual trigraph character

                current_character = replacement;
            }
        }

        if current_character == '\\' && self.chars.peek() == Some(&'\n') {
            length += self.chars.next().unwrap().len_utf8();

            self.current_pos += length; // \ and ?
            return self.next();
        } else {
            self.current_pos = start + length;
            return Some((start..self.current_pos, current_character));
        }
    }
}

impl<'a> SkipEscapedNewlinesIter<'a> {
    fn new(source: &'a str) -> SkipEscapedNewlinesIter<'a> {
        SkipEscapedNewlinesIter {
            chars: source.chars().peekmore(),
            current_pos: 0,
        }
    }
}
fn main() {
    let source = "?\\\n?=";
    let i = SkipEscapedNewlinesIter {
        current_pos: 0,
        chars: source.chars().peekmore(),
    };
    let tokens = i.collect::<Vec<_>>();
    dbg!(&tokens);
    for (span, token) in tokens {
        println!(
            "{:4?} @ {:#?} -> {:2?} ",
            &source[span.clone()],
            span.clone(),
            token
        );
    }
}
