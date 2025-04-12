use cyntax_common::ast::Punctuator;
use cyntax_common::ast::Token;
use cyntax_common::ast::Whitespace;
use cyntax_common::spanned::Spanned;
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
        Spanned { value: $p, range: $r }
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
            first_character @ span!(nondigit!()) => {
                let identifier = self.lex_identifier(&first_character);
                Some(identifier.map(|identifier| Token::Identifier(identifier)))
            }

            // Literals
            span!(range, '"') => {
                let string = self.lex_string_literal(range);
                Some(string.map(|string| Token::StringLiteral(string)))
            }
            span!(range, '\'') => {
                let string = self.lex_char_literal(range);
                Some(string.map(|string| Token::CharLiteral(string)))
            }
            first_character @ span!(digit!()) => {
                let number = self.lex_number(&first_character);
                Some(number.map(|num| Token::PPNumber(num)))
            }
            // Digits can start with 0
            first_character @ span!('.') if matches!(self.chars.peek(), Some(span!(digit!()))) => {
                let number = self.lex_number(&first_character);

                Some(number.map(|num| Token::PPNumber(num)))
            }
            // span!(range, c@ ('(' | ')' | '{' | '}' | '[' | ']')) if self.ignore_delimiters => Some(
            //     Spanned::new(range, Token::Punctuator(Punctuator::from_char(c).unwrap())),
            // ),
            // Entirely skip over line comments
            span!('/') if matches!(self.chars.peek(), Some(span!('/'))) => {
                while let Some(span!(char_span, char)) = self.chars.next() {
                    if char == '\n' {
                        return Some(Spanned::new(char_span, Token::Whitespace(Whitespace::Newline)));
                    }
                }
                self.next()
            }
            //inline comments
            span!('/') if matches!(self.chars.peek(), Some(span!('*'))) => {
                while let Some(span!(char_span, char)) = self.chars.next() {
                    if char == '*' && matches!(self.chars.peek(), Some(span!('/'))) {
                        return Some(Spanned::new(char_span, Token::Whitespace(Whitespace::Newline)));
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
                    } else if matches!(token, '/') && matches!(self.chars.peek_nth(1), Some(span!('/'))) {
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
            span!(range, '#') if matches!(self.chars.peek(), Some(span!('#'))) => {
                self.chars.next().unwrap();
                Some(Spanned::new(range, Token::Punctuator(Punctuator::HashHash)))
            }
            span!(range, '.') if matches!(self.chars.peek_nth(0), Some(span!('.'))) && matches!(self.chars.peek_nth(1), Some(span!('.'))) => {
                self.chars.next().unwrap();
                let last_dot = self.chars.next().unwrap();
                Some(Spanned::new(range.start..last_dot.range.end, Token::Punctuator(Punctuator::DotDotDot)))
            }
            span!(range, punctuator) if Punctuator::is_punctuation(punctuator) => Some(Spanned::new(range, Token::Punctuator(Punctuator::from_char(punctuator).unwrap()))),
            span!(range, ' ') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Space))),
            span!(range, '\t') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Tab))),
            span!(range, '\n') => Some(Spanned::new(range, Token::Whitespace(Whitespace::Newline))),
            ch => {
                self.fatal_diagnostic(cyntax_errors::errors::SimpleError(ch.range, format!("unimplemented character {}", ch.value)))
            }
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
    pub fn lex_identifier(&mut self, first_character: &Spanned<char>) -> Spanned<String> {
        // let mut ranges = vec![first_character.start..first_character.end];
        let mut identifier = String::from(first_character.value);
        let mut previous_end = first_character.range.end;

        while let Some(span!(identifier!())) = self.chars.peek() {
            let next = self.chars.next().unwrap();
            previous_end = next.range.end;
            identifier.push(next.value);
        }
        Spanned::new(first_character.range.start..previous_end, identifier)
    }
    pub fn lex_string_literal(&mut self, range: CharLocation) -> Spanned<String> {
        let mut ranges = String::new();
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
            ranges.push(next.value);
        }

        Spanned::new(range.start..end, ranges)
    }
    pub fn lex_char_literal(&mut self, range: CharLocation) -> Spanned<String> {
        let mut ranges = String::new();
        let mut end = range.end;

        while let Some(span!(c)) = self.chars.peek() {
            if *c == '\'' {
                let end_quote = self.chars.next().unwrap();
                end = end_quote.range.end;
                break;
            }
            // Handle escaped characters within string literal
            if *c == '\\' && matches!(self.chars.peek_nth(1).unwrap(), span!('\'' | '\\')) {
                // Skip \
                self.chars.next().unwrap();
            }

            let next = self.chars.next().unwrap();
            end = next.range.end;
            ranges.push(next.value);
        }

        Spanned::new(range.start..end, ranges)
    }
    pub fn lex_number(&mut self, first_character: &Spanned<char>) -> Spanned<String> {
        let start = first_character.range.start;
        let mut end = first_character.range.end;
        let mut number = String::from(first_character.value);

        let mut expecting_exponent = false;
        while let Some(span!(c)) = self.chars.peek() {
            match c {
                'e' | 'E' | 'p' | 'P' => {
                    expecting_exponent = true;
                    // e / E / p / P
                    number.push(self.chars.next().unwrap().value);
                }
                '+' | '-' if expecting_exponent => {
                    expecting_exponent = false;
                    let sign = self.chars.next().unwrap();
                    end = sign.range.end;
                    number.push(sign.value);
                }
                '.' | digit!() => {
                    let dot_or_digit = self.chars.next().unwrap();
                    end = dot_or_digit.range.end;
                    number.push(dot_or_digit.value);
                }

                // Identifier parts must start with a nondigit, otherwise it would just... be a part of the preceeding number
                nondigit!() => {
                    let nondigit = self.chars.next().unwrap();
                    end = nondigit.range.end;
                    let identifier = self.lex_identifier(&nondigit);
                    match identifier.value.chars().last().as_ref() {
                        Some('e' | 'E' | 'p' | 'P') => {
                            expecting_exponent = true;
                        }
                        _ => {}
                    }

                    number.push_str(&identifier.value);
                }
                _ => break,
            }
        }

        Spanned::new(start..end, number)
    }
}
// Util functions
impl<'src> Lexer<'src> {
    pub fn fatal_diagnostic<E: cyntax_errors::Diagnostic>(&mut self, diagnostic: E) -> !{
        panic!("{}", diagnostic.into_why_report().with(self.file_name, self.source))

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
