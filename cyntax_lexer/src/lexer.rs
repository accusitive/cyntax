use cyntax_common::ast::Punctuator;
use cyntax_common::ast::Token;
use cyntax_common::ast::Whitespace;
use cyntax_common::spanned::Spanned;
use cyntax_common::sparsechars::SparseChars;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

use crate::prelexer::PrelexerIter;
use std::ops::Range;

// Identifier safe characters
#[macro_export]
macro_rules! identifier {
    () => {
        nondigit!() | digit!()
    };
}
#[macro_export]
macro_rules! nondigit {
    () => {
        '_' | 'A'..='Z' | 'a'..='z'
    };
}
#[macro_export]
macro_rules! digit {
    () => {
        '0'..='9'
    };
}
#[macro_export]
macro_rules! opening_delimiter {
    () => {
        '(' | '{' | '['
    };
}

#[macro_export]
macro_rules! span {
    ($r: pat, $p: pat) => {
        Spanned {
            value: $p,
            range: $r,
        }
    };
    ($p: pat) => {
        Spanned { value: $p, .. }
    };
}
pub use digit;
pub use identifier;
pub use nondigit;
pub use opening_delimiter;
pub use span;
/// Characters can span multiple characters, so a simple `usize` index is not enough.
/// an extreme example is a trigraph. the source `??=`, is lexed as only 1 character, that spans from 0..2
pub type CharLocation = Range<usize>;

#[derive(Debug)]
pub struct Lexer<'src> {
    pub chars: PeekMoreIterator<PrelexerIter<'src>>,
    pub file_name: &'src str,
    pub source: &'src str,
    at_start_of_line: bool,
    inside_control_line: bool,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.chars.next()?;
        let is_newline = matches!(next.value, '\n');

        let token = match next {
            span!(range, nondigit!()) => {
                let identifier = self.lex_identifier(&range);
                Some(identifier.map(|identifier| Token::Identifier(identifier)))
            }

            // Literals
            span!(range, '"') => {
                let string = self.lex_string_literal(range);
                Some(string.map(|string| Token::StringLiteral(string)))
            }
            span!(range, digit!()) => {
                let number = self.lex_number(range);
                Some(number.map(|num| Token::PPNumber(num)))
            }
            // Digits can start with 0
            span!(dot_range, '.') if matches!(self.chars.peek(), Some(span!(digit!()))) => {
                let number = self.lex_number(dot_range);

                Some(number.map(|num| Token::PPNumber(num)))
            }
            // span!(range, c@ ('(' | ')' | '{' | '}' | '[' | ']')) if self.ignore_delimiters => Some(
            //     Spanned::new(range, Token::Punctuator(Punctuator::from_char(c).unwrap())),
            // ),
            span!(range, opening_delimiter @ opening_delimiter!()) => {
                let token = self.lex_delimited(&range, opening_delimiter);
                Some(token)
            }
            // Entirely skip over line comments
            span!('/') if matches!(self.chars.peek(), Some(span!('/'))) => {
                while let Some(span!(char)) = self.chars.next() {
                    if char == '\n' {
                        break;
                    }
                }
                self.next()
            }
            span!(range, '#') if self.at_start_of_line => {
                let mut tokens = vec![];
                let mut end = range.end;
                let mut add = true;
                while let Some(span!(token)) = self.chars.peek() {
                    if matches!(token, '\n') {
                        break;
                    } else if matches!(token, '/')
                        && matches!(self.chars.peek_nth(1), Some(span!('/')))
                    {
                        add = false;
                        self.chars.next().unwrap();
                        self.chars.next().unwrap();
                    } else {
                        self.inside_control_line = true;
                        let n = self.next().unwrap();
                        self.inside_control_line = false;
                        if add {
                            end = n.range.end;
                            tokens.push(n);
                        }
                    }
                }
                Some(Spanned {
                    value: Token::ControlLine(tokens),
                    range: range.start..end,
                })
            }
            span!(range, punctuator) if Punctuator::is_punctuation(punctuator) => {
                Some(Spanned::new(
                    range,
                    Token::Punctuator(Punctuator::from_char(punctuator).unwrap()),
                ))
            }
            span!(range, ' ') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Space))),
            span!(range, '\t') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Tab))),
            span!(range, '\n') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Newline))),
            ch => unimplemented!("character {} is not implemented", ch.value),
        };

        // Set this after lexing the token, otherwise it would always be false for non-newline tokens
        self.at_start_of_line = is_newline;

        token
    }
}
impl<'src> Lexer<'src> {
    pub fn new(file_name: &'src str, source: &'src str) -> Lexer<'src> {
        Lexer {
            chars: PrelexerIter::new(source).peekmore(),
            file_name,
            source,
            at_start_of_line: true,
            inside_control_line: false,
        }
    }
    pub fn lex_identifier(&mut self, first_character: &CharLocation) -> Spanned<SparseChars> {
        let mut ranges = vec![first_character.start..first_character.end];
        let mut previous_end = first_character.end;

        while let Some(span!(identifier!())) = self.chars.peek() {
            let next = self.chars.next().unwrap();
            previous_end = next.range.end;
            ranges.push(next.range);
        }
        Spanned::new(
            first_character.start..previous_end,
            SparseChars::new(ranges),
        )
    }
    pub fn lex_string_literal(&mut self, range: CharLocation) -> Spanned<Vec<CharLocation>> {
        let mut ranges = vec![];
        let mut end = range.end;

        while let Some(span!(c)) = self.chars.peek() {
            if *c == '"' {
                let end_quote = self.chars.next().unwrap();
                end = end_quote.range.end;
                break;
            }
            // Handle escaped characters within string literal
            if *c == '\\' && matches!(self.chars.peek_nth(1).unwrap(), span!('"' | '\\')) {
                // Skip \
                self.chars.next().unwrap();
            }

            let next = self.chars.next().unwrap();
            end = next.range.end;
            ranges.push(next.range);
        }

        Spanned::new(range.start..end, ranges)
    }
    pub fn lex_number(&mut self, first_character: CharLocation) -> Spanned<SparseChars> {
        let start = first_character.start;
        let mut end = first_character.end;
        let mut number = vec![first_character];

        let mut expecting_exponent = false;
        while let Some(span!(c)) = self.chars.peek() {
            match c {
                'e' | 'E' | 'p' | 'P' => {
                    expecting_exponent = true;
                    // e / E / p / P
                    number.push(self.chars.next().unwrap().range);
                }
                '+' | '-' if expecting_exponent => {
                    expecting_exponent = false;
                    let sign = self.chars.next().unwrap();
                    end = sign.range.end;
                    number.push(sign.range);
                }
                '.' | digit!() => {
                    let dot_or_digit = self.chars.next().unwrap();
                    end = dot_or_digit.range.end;
                    number.push(dot_or_digit.range);
                }

                // Identifier parts must start with a nondigit, otherwise it would just... be a part of the preceeding number
                nondigit!() => {
                    let nondigit = self.chars.next().unwrap();
                    end = nondigit.range.end;
                    let identifier = self.lex_identifier(&nondigit.range);
                    match identifier
                        .value
                        .last
                        .as_ref()
                        .map(|c| &self.source[c.start..c.end])
                    {
                        Some("e" | "E" | "p" | "P") => {
                            expecting_exponent = true;
                        }
                        _ => {}
                    }

                    number.extend(identifier.value.iter());
                }
                _ => break,
            }
        }

        Spanned::new(start..end, SparseChars::new(number))
    }
    pub fn lex_delimited(
        &mut self,
        range: &CharLocation,
        opening_delimiter: char,
    ) -> Spanned<Token> {
        let mut tokens = vec![];
        let closing_delimiter = Self::closing_delimiter_for(opening_delimiter);
        let mut closed = false;
        let mut end = range.end;
        while let Some(span!(c)) = self.chars.peek() {
            if self.inside_control_line && *c == '\n' {
                break;
            }
            if *c == closing_delimiter {
                closed = true;
                self.next().unwrap();
                break;
            } else {
                let next = self.next().unwrap();
                end = next.range.end;
                tokens.push(next);
            }
        }

        dbg!(&closed, self.inside_control_line);
        if !closed && self.inside_control_line {
            return Spanned::new(
                range.start..end,
                Token::Delimited {
                    opener: opening_delimiter,
                    closer: None,
                    inner_tokens: Spanned::new(range.start..end, tokens),
                },
            );
        }
        if !closed && !self.inside_control_line {
            let err = cyntax_errors::errors::UnmatchedDelimiter {
                opening_delimiter_location: range.start..range.end,
                potential_closing_delimiter_location: end,
                closing_delimiter,
            };
            self.fatal_diagnostic(err);
        }
        return Spanned::new(
            range.start..end,
            Token::Delimited {
                opener: opening_delimiter,
                closer: Some(closing_delimiter),
                inner_tokens: Spanned::new(range.start..end, tokens),
            },
        );
    }
}
// enum LexDelimitedResult {
//     Delimited(char, Spanned<Vec<Spanned<Token>>>),
//     UnmatchedButItsFine(Spanned<Vec<Spanned<Token>>>)
// }
// Util functions
impl<'src> Lexer<'src> {
    pub fn fatal_diagnostic<E: cyntax_errors::Diagnostic>(&mut self, diagnostic: E) {
        panic!(
            "{}",
            diagnostic
                .into_why_report()
                .with(self.file_name, self.source)
        );
        // println!(
        //     "{}",
        //     cyntax_errors::write_codespan_report(
        //         diagnostic.into_codespan_report(),
        //         self.file_name,
        //         self.source
        //     )
        // );
        // panic!();
    }
    pub fn closing_delimiter_for(c: char) -> char {
        match c {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            _ => unreachable!(),
        }
    }
    pub fn ignore_preceeding_whitespace<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        while let Some(span!(' ' | '\t')) = self.chars.peek() {
            self.chars.next().unwrap();
        }
        f(self)
    }
}
