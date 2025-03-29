use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

use crate::Punctuator;
use crate::Token;
use crate::Whitespace;
use crate::prelexer::PrelexerIter;
use std::{iter::Peekable, ops::Range};

macro_rules! identifier {
    () => {
        nondigit!() | digit!()
    };
}
macro_rules! nondigit {
    () => {
        '_' | 'A'..='Z' | 'a'..='z'
    };
}
macro_rules! digit {
    () => {
        '0'..='9'
    };
}
#[derive(Debug)]
pub struct Lexer<'a> {
    pub chars: PeekMoreIterator<PrelexerIter<'a>>,
    pub source: &'a str,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next()? {
            (range, nondigit!()) => {
                let identifier = self.lex_identifier(range);
                Some((identifier.0, Token::Identifier(identifier.1)))
            }

            (range, '"') => {
                let mut ranges = vec![];
                let start = range.start;
                let mut end = range.end;

                while let Some((_, c)) = self.chars.peek() {
                    if *c == '"' {
                        let end_quote = self.chars.next().unwrap();
                        end = end_quote.0.end;
                        break;
                    }
                    // Handle escaped characters within string literal
                    if *c == '\\' && matches!(self.chars.peek_nth(1).unwrap().1, '"' | '\\') {
                        // Skip \
                        self.chars.next().unwrap();
                    }

                    let next = self.chars.next().unwrap();
                    end = next.0.end;
                    ranges.push(next.0);
                }
                Some((start..end, Token::StringLiteral(ranges)))
            }

            (range, '.') if matches!(self.chars.peek().map(|t| t.1), Some('0'..='9')) => {
                Some(self.lex_number(range))
            }
            (range, digit!()) => Some(self.lex_number(range)),
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
    pub fn lex_identifier(
        &mut self,
        first_character: Range<usize>,
    ) -> (Range<usize>, Vec<Range<usize>>) {
        let mut ranges = vec![first_character.clone()];
        let mut previous_end = first_character.end;

        while let Some((_, identifier!())) = self.chars.peek() {
            let next = self.chars.next().unwrap();
            previous_end = next.0.end;
            ranges.push(next.0);
        }
        (first_character.start..previous_end, ranges)
    }
    pub fn lex_number(&mut self, first_character: Range<usize>) -> (Range<usize>, Token) {
        let start = first_character.start;
        let mut end = first_character.end;
        let mut number = vec![first_character];

        // Whether the last part of the pp-num was an identifier, and if so, did it end with e | E | p | P
        let mut last_identifier_starts_exponent = false;
        while let Some((_, c)) = self.chars.peek().cloned() {
            match c {
                'e' | 'E' | 'p' | 'P' => {
                    last_identifier_starts_exponent = true;
                    number.push(self.chars.next().unwrap().0); // e / E / p / P
                    continue;
                }
                // 'e' | 'E' | 'p' | 'P' if matches!(self.chars.peek_nth(1), Some((_, '+' | '-'))) => {
                //     number.push(self.chars.next().unwrap().0); // e / E / p / P
                //     let sign = self.chars.next().unwrap();
                //     end = sign.0.end;
                //     number.push(sign.0);
                // }
                '+' | '-' if last_identifier_starts_exponent => {
                    last_identifier_starts_exponent = false;
                    let sign = self.chars.next().unwrap();
                    end = sign.0.end;
                    number.push(sign.0);
                }
                '.' => {
                    let dot = self.chars.next().unwrap();
                    end = dot.0.end;
                    number.push(dot.0);
                }
                digit!() => {
                    let digit = self.chars.next().unwrap();
                    end = digit.0.end;
                    number.push(digit.0);
                }

                // Identifier parts must start with a nondigit, otherwise it would just... be a part of the preceeding number
                nondigit!() => {
                    let nondigit = self.chars.next().unwrap();
                    end = nondigit.0.end;
                    let identifier = self.lex_identifier(nondigit.0);
                    match identifier.1.last().map(|c| &self.source[c.clone()]) {
                        Some("e" | "E" | "p" | "P") => {
                            last_identifier_starts_exponent = true;
                        },
                        _ => {}
                    }

                    number.extend(identifier.1);
                }
                _ => break,
            }
        }

        (start..end, Token::PPNumber(number))
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
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Identifier(_))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_identifier_before() {
    let source = "world\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Identifier(_))
    ));
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
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Punctuator(Punctuator::Semicolon))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_punctuator_before() {
    let source = ";\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Punctuator(Punctuator::Semicolon))
    ));
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
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Whitespace(Whitespace::Space))
    ));
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
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Whitespace(Whitespace::Space))
    ));
    assert_eq!(lexer.next(), None);
}
