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
                }
                '+' | '-' if last_identifier_starts_exponent => {
                    last_identifier_starts_exponent = false;
                    let sign = self.chars.next().unwrap();
                    end = sign.0.end;
                    number.push(sign.0);
                }
                '.' | digit!() => {
                    let dot_or_digit = self.chars.next().unwrap();
                    end = dot_or_digit.0.end;
                    number.push(dot_or_digit.0);
                }

                // Identifier parts must start with a nondigit, otherwise it would just... be a part of the preceeding number
                nondigit!() => {
                    let nondigit = self.chars.next().unwrap();
                    end = nondigit.0.end;
                    let identifier = self.lex_identifier(nondigit.0);
                    match identifier.1.last().map(|c| &self.source[c.clone()]) {
                        Some("e" | "E" | "p" | "P") => {
                            last_identifier_starts_exponent = true;
                        }
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
