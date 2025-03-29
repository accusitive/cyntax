use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

use crate::Punctuator;
use crate::Token;
use crate::Whitespace;
use crate::prelexer::PrelexerIter;
use std::{iter::Peekable, ops::Range};

#[derive(Debug)]
pub struct Lexer<'a> {
    pub chars: PeekMoreIterator<PrelexerIter<'a>>,
    pub source: &'a str,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next()? {
            (range, 'A'..='z') => {
                let mut ranges = vec![range.clone()];
                let mut previous_end = range.end;

                while let Some((_, 'A'..='z' | '0'..='9')) = self.chars.peek() {
                    let next = self.chars.next().unwrap();
                    previous_end = next.0.end;
                    ranges.push(next.0);
                }
                Some((range.start..previous_end, Token::Identifier(ranges)))
            }

            (range, '"') => {
                let mut ranges = vec![];
                let mut previous_end = range.end;

                while let Some((_, c)) = self.chars.peek() {
                    if *c == '"' {
                        let end_quote = self.chars.next().unwrap();
                        previous_end = end_quote.0.end;
                        break;
                    }
                    if *c == '\\' && matches!(self.chars.peek_nth(1).unwrap().1, '"'| '\\') {
                        // Skip \
                        self.chars.next().unwrap();
                    }

                    let next = self.chars.next().unwrap();
                    previous_end = next.0.end;
                    ranges.push(next.0);
                }
                Some((range.start..previous_end, Token::StringLiteral(ranges)))
            }
            (range, punctuator) if Punctuator::is_punctuation(punctuator) => Some((
                range,
                Token::Punctuator(Punctuator::from_char(punctuator).unwrap()),
            )),

            (range, ' ') => Some((range, Token::Whitespace(Whitespace::Space))),
            (range, '\t') => Some((range, Token::Whitespace(Whitespace::Tab))),
            (range, '\n') => Some((range, Token::Whitespace(Whitespace::Newline))),
            ch => unimplemented!("character {} is not implemented", ch.1),
        }
    }
}
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            chars: PrelexerIter::new(source).peekmore(),
            source,
        }
    }
}
fn test_helper(source: &str, ranges: &[Range<usize>]) -> String {
    let mut s = String::new();
    for range in ranges {
        s.push_str(&source[range.clone()]);
    }
    s
}

#[test]
fn test_empty_string_literal() {
    let source = "\"\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_simple_string_literal() {
    let source = "\"hello\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_spaces() {
    let source = "\"hello world\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello world".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_escaped_quote() {
    let source = "\"hello\\\"world\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello\"world".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_multiple_escaped_quotes() {
    let source = "\"hello\\\"world\\\"test\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello\"world\"test".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_other_chars() {
    let source = "\"hello123world!@#\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello123world!@#".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_identifier_after() {
    let source = "\"hello\"world";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Identifier(_))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_identifier_before() {
    let source = "world\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Identifier(_))));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_punctuator_after() {
    let source = "\"hello\";";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Punctuator(Punctuator::Semicolon))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_punctuator_before() {
    let source = ";\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Punctuator(Punctuator::Semicolon))));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_whitespace_before() {
    let source = " \"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Whitespace(Whitespace::Space))));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_whitespace_after() {
    let source = "\"hello\" ";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(lexer.next().map(|t|t.1), Some(Token::Whitespace(Whitespace::Space))));
    assert_eq!(lexer.next(), None);
}