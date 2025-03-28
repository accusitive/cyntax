use std::{iter::Peekable, ops::Range};
use crate::prelexer::PrelexerIter;

#[derive(Debug)]
pub struct Lexer<'a> {
    pub chars: Peekable<PrelexerIter<'a>>,
    pub source: &'a str,
}
#[derive(Debug)]
pub enum Token {
    Identifier(Vec<Range<usize>>),
    Whitespace(Whitespace),
    Punctuator(Punctuator),
}
#[derive(Debug)]
pub enum Whitespace {
    /// ` `
    Space,
    /// \n
    Newline,
    /// \t
    Tab,
}
#[derive(Debug)]
pub enum Punctuator {
    LParen,
    RParen,
}
impl<'a> Iterator for Lexer<'a> {
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next()? {
            (range, 'A'..'z') => {
                let mut v = vec![range.clone()];
                // Glue consecutive token stretches together
                let mut previous_end = range.end;
                while let Some((range, 'A'..='z' | '0'..='9')) = self.chars.peek() {
                    if v.len() > 0 && range.start == previous_end {
                        v.last_mut().unwrap().end = range.end;
                        previous_end = range.end;
                        self.chars.next().unwrap();
                    } else {
                        previous_end = range.end;
                        v.push(self.chars.next().unwrap().0);
                    }
                }
                dbg!(&v);
                Some((range.start..previous_end, Token::Identifier(v)))
            }
            (range, '(') => Some((range, Token::Punctuator(Punctuator::LParen))),
            (range, ')') => Some((range, Token::Punctuator(Punctuator::RParen))),

            (range, ' ') => Some((range, Token::Whitespace(Whitespace::Space))),
            (range, '\t') => Some((range, Token::Whitespace(Whitespace::Tab))),
            (range, '\n') => Some((range, Token::Whitespace(Whitespace::Newline))),
            _ => unimplemented!(),
        }
    }
}
