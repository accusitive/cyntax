use cab_why::Label;
use cab_why::LabelSeverity;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

use crate::Directive;
use crate::Punctuator;
use crate::StrPieces;
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
    at_start_of_line: bool,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.chars.next()?;
        let is_newline = matches!(next.1, '\n');

        let token = match next {
            (range, nondigit!()) => {
                let identifier: (Range<usize>, Vec<Range<usize>>) = self.lex_identifier(&range);
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
            (dot_range, '.') if matches!(self.chars.peek().map(|t| t.1), Some(digit!())) => {
                let number = self.lex_number(dot_range);

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
            // Entirely skip over line comments
            (_, '/') if matches!(self.chars.peek(), Some((_, '/'))) => {
                while let Some((_, char)) = self.chars.next() {
                    if char == '\n' {
                        break;
                    }
                }
                self.next()
            }
            (range, '#') if self.at_start_of_line => {
                if matches!(self.chars.peek(), Some((_, nondigit!()))) {
                    let directive = self.ignore_preceeding_whitespace(|this| {
                        let (first, _) = this.chars.next().unwrap();
                        this.lex_identifier(&first)
                    });

                    if self.is_equal_within_source(&directive.1, "define") {
                        let macro_name = self.ignore_preceeding_whitespace(|this| {
                            let (first, _) = this.chars.next().unwrap();
                            this.lex_identifier(&first)
                        });
                        if matches!(self.chars.peek(), Some((_, '('))) {
                            let (lparen_range, _) = self.chars.next().unwrap();
                            let (_, _, parameter_list) = self.lex_delimited(lparen_range, '(');
                            let replacement_list = self.ignore_preceeding_whitespace(|this| {
                                this.take_while(|c| c.1 != Token::Whitespace(Whitespace::Newline))
                                    .collect::<Vec<_>>()
                            });
                            Some((
                                range.start..macro_name.0.end,
                                Token::Directive(Directive::DefineFunction(
                                    macro_name,
                                    parameter_list,
                                    replacement_list,
                                )),
                            ))
                        } else {
                            let replacement_list = self.ignore_preceeding_whitespace(|this| {
                                this.take_while(|c| c.1 != Token::Whitespace(Whitespace::Newline))
                                    .collect::<Vec<_>>()
                            });
                            Some((
                                range.start..macro_name.0.end,
                                Token::Directive(Directive::DefineObject(
                                    macro_name,
                                    replacement_list,
                                )),
                            ))
                        }
                    } else if self.is_equal_within_source(&directive.1, "undef") {
                        let macro_name = self.ignore_preceeding_whitespace(|this| {
                            let (first, _) = this.chars.next().unwrap();
                            this.lex_identifier(&first)
                        });
                        Some((
                            range.start..macro_name.0.end,
                            Token::Directive(Directive::Undefine(macro_name)),
                        ))
                    } else {
                        unimplemented!()
                    }
                } else if matches!(self.chars.peek(), Some((_, '\n'))) {
                    self.next()
                } else {
                    todo!()
                }
            }
            (range, punctuator) if Punctuator::is_punctuation(punctuator) => Some((
                range,
                Token::Punctuator(Punctuator::from_char(punctuator).unwrap()),
            )),
            (range, ' ') => Some((range, Token::Whitespace(Whitespace::Space))),
            (range, '\t') => Some((range, Token::Whitespace(Whitespace::Tab))),
            (range, '\n') => Some((range, Token::Whitespace(Whitespace::Newline))),
            ch => unimplemented!("character {} is not implemented", ch.1),
        };

        // Set this after lexing the token, otherwise it would always be false for non-newline tokens
        self.at_start_of_line = is_newline;
        dbg!(&self.at_start_of_line);

        token
    }
}
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            chars: PrelexerIter::new(source).peekmore(),
            source,
            at_start_of_line: true,
        }
    }
    pub fn lex_identifier(
        &mut self,
        first_character: &Range<usize>,
    ) -> (Range<usize>, Vec<Range<usize>>) {
        let mut ranges = vec![first_character.start..first_character.end];
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
        while let Some((_, c)) = self.chars.peek() {
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
                    let identifier = self.lex_identifier(&nondigit.0);
                    match identifier.1.last().map(|c| &self.source[c.start..c.end]) {
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
    pub fn ignore_preceeding_whitespace<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        while let Some((_, ' ' | '\t')) = self.chars.peek() {
            self.next().unwrap();
        }
        f(self)
    }
    pub fn is_equal_within_source(&self, left: &[Range<usize>], right: &str) -> bool {
        let left = left
            .iter()
            .flat_map(|range| self.source[range.start..range.end].chars());
        let right = right.chars();

        left.eq(right)
    }
}
