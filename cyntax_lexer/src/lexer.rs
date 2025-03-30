use cab_why::Label;
use cab_why::LabelSeverity;
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
macro_rules! opening_delimiter {
    () => {
        '(' | '{' | '['
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
                let identifier: (Range<usize>, Vec<Range<usize>>) = self.lex_identifier(range);
                Some((identifier.0, Token::Identifier(identifier.1)))
            }

            // Literals
            (range, '"') => {
                let string = self.lex_string_literal(range);
                Some((string.0, Token::StringLiteral(string.1)))
            }
            (range, digit!()) => {
                let number = self.lex_number(range);
                Some((number.0, Token::PPNumber(number.1)))
            }
            // Digits can start with 0
            (range, '.') if matches!(self.chars.peek().map(|t| t.1), Some(digit!())) => {
                let number = self.lex_number(range);

                Some((number.0, Token::PPNumber(number.1)))
            }
            (range, opening_delimiter @ opening_delimiter!()) => {
                let (closing_delimiter, range, tokens) =
                    self.lex_delimited(range, opening_delimiter);
                Some((
                    range,
                    Token::Delimited(opening_delimiter, closing_delimiter, tokens),
                ))
            }

            // Entirely skip over comments
            (_, '/') if matches!(self.chars.peek(), Some((_, '/'))) => {
                while let Some((_, char)) = self.chars.next() {
                    if char == '\n' {
                        break;
                    }
                }
                self.next()
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
    pub fn lex_string_literal(&mut self, range: Range<usize>) -> (Range<usize>, Vec<Range<usize>>) {
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

        (start..end, ranges)
    }
    pub fn lex_number(
        &mut self,
        first_character: Range<usize>,
    ) -> (Range<usize>, Vec<Range<usize>>) {
        let start = first_character.start;
        let mut end = first_character.end;
        let mut number = vec![first_character];

        let mut expecting_exponent = false;
        while let Some((_, c)) = self.chars.peek().cloned() {
            match c {
                'e' | 'E' | 'p' | 'P' => {
                    expecting_exponent = true;
                    // e / E / p / P
                    number.push(self.chars.next().unwrap().0);
                }
                '+' | '-' if expecting_exponent => {
                    expecting_exponent = false;
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
                            expecting_exponent = true;
                        }
                        _ => {}
                    }

                    number.extend(identifier.1);
                }
                _ => break,
            }
        }

        (start..end, number)
    }
    pub fn lex_delimited(
        &mut self,
        range: Range<usize>,
        opening_delimiter: char,
    ) -> (char, Range<usize>, Vec<(Range<usize>, Token)>) {
        let mut tokens = vec![];
        let closing_delimiter = Self::closing_delimiter_for(opening_delimiter);
        let mut closed = false;
        let mut end = range.end;
        while let Some((_, c)) = self.chars.peek() {
            if *c == closing_delimiter {
                closed = true;
                self.next().unwrap();
                break;
            } else {
                let next = self.next().unwrap();
                end = next.0.end;
                tokens.push((next.0, next.1));
            }
        }

        if !closed {
            let mut report =
                cab_why::Report::new(cab_why::ReportSeverity::Error, "Unmatched delimiter");
            report.push_label(Label::new(
                range.start..range.end,
                "Unmatched delimiter",
                cab_why::LabelSeverity::Primary,
            ));
            report.push_label(Label::secondary(
                end..end,
                "Potential location for a closing delimiter",
            ));
            report.push_help(format!("Add an ending delimiter `{}`", closing_delimiter));
            panic!("{}", report.with("test.c", self.source));
        }
        (closing_delimiter, range.start..end, tokens)
    }
}
// Util functions
impl<'a> Lexer<'a> {
    pub fn closing_delimiter_for(c: char) -> char {
        match c {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            _ => unreachable!(),
        }
    }
}
