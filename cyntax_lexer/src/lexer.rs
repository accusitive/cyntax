use cab_why::Label;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

use crate::Directive;
use crate::Punctuator;
use crate::Token;
use crate::Whitespace;
use crate::prelexer::PrelexerIter;
use crate::spanned::Spanned;
use std::ops::Range;

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
/// Characters can span multiple characters, so a simple `usize` index is not enough.
/// an extreme example is a trigraph. the source `??=`, is lexed as only 1 character, that spans from 0..2
pub type CharLocation = Range<usize>;

#[derive(Debug)]
pub struct Lexer<'a> {
    pub chars: PeekMoreIterator<PrelexerIter<'a>>,
    pub source: &'a str,
    at_start_of_line: bool,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.chars.next()?;
        let is_newline = matches!(next.value, '\n');

        let token = match next {
            span!(range, nondigit!()) => {
                let identifier = self.lex_identifier(&range);
                Some(identifier.augment(|identifier| Token::Identifier(identifier)))
            }

            // Literals
            span!(range, '"') => {
                let string = self.lex_string_literal(range);
                Some(string.augment(|string| Token::StringLiteral(string)))
            }
            span!(range, digit!()) => {
                let number = self.lex_number(range);
                Some(number.augment(|num| Token::PPNumber(num)))
            }
            // Digits can start with 0
            span!(dot_range, '.') if matches!(self.chars.peek(), Some(span!(digit!()))) => {
                let number = self.lex_number(dot_range);

                Some(number.augment(|num| Token::PPNumber(num)))
            }
            span!(range, opening_delimiter @ opening_delimiter!()) => {
                let (closing_delimiter, inner_tokens) =
                    self.lex_delimited(&range, opening_delimiter);
                Some(Spanned::new(
                    range,
                    Token::Delimited(opening_delimiter, closing_delimiter, inner_tokens),
                ))
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
            span!(range, '#') if self.at_start_of_line => self.lex_directive(&range),
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
    pub fn lex_identifier(&mut self, first_character: &CharLocation) -> Spanned<Vec<CharLocation>> {
        let mut ranges = vec![first_character.start..first_character.end];
        let mut previous_end = first_character.end;

        while let Some(span!(identifier!())) = self.chars.peek() {
            let next = self.chars.next().unwrap();
            previous_end = next.range.end;
            ranges.push(next.range);
        }
        Spanned::new(first_character.start..previous_end, ranges)
    }
    pub fn lex_string_literal(&mut self, range: CharLocation) -> Spanned<Vec<CharLocation>> {
        let mut ranges = vec![];
        let start = range.start;
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

        Spanned::new(start..end, ranges)
    }
    pub fn lex_number(&mut self, first_character: CharLocation) -> Spanned<Vec<CharLocation>> {
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
                        .last()
                        .map(|c| &self.source[c.start..c.end])
                    {
                        Some("e" | "E" | "p" | "P") => {
                            expecting_exponent = true;
                        }
                        _ => {}
                    }

                    number.extend(identifier.value);
                }
                _ => break,
            }
        }

        Spanned::new(start..end, number)
    }
    pub fn lex_delimited(
        &mut self,
        range: &CharLocation,
        opening_delimiter: char,
    ) -> (char, Spanned<Vec<Spanned<Token>>>) {
        let mut tokens = vec![];
        let closing_delimiter = Self::closing_delimiter_for(opening_delimiter);
        let mut closed = false;
        let mut end = range.end;
        while let Some(span!(c)) = self.chars.peek() {
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
        (closing_delimiter, Spanned::new(range.start..end, tokens))
    }
    pub fn lex_directive(&mut self, range: &CharLocation) -> Option<Spanned<Token>> {
        let is_empty_directive = !matches!(self.chars.peek(), Some(span!(nondigit!())));

        if !is_empty_directive {
            let directive = self.ignore_preceeding_whitespace(|this| {
                let span!(first, _) = this.chars.next().unwrap();
                this.lex_identifier(&first)
            });

            if self.is_equal_within_source(&directive.value, "define") {
                self.lex_define_directive(range)
            } else if self.is_equal_within_source(&directive.value, "undef") {
                self.lex_undefine_direcrive(range)
            } else {
                unimplemented!()
            }
        } else if matches!(self.chars.peek(), Some(span!('\n'))) {
            self.next()
        } else {
            None
        }
    }
    pub fn lex_define_directive(&mut self, range: &CharLocation) -> Option<Spanned<Token>> {
        let macro_name = self.ignore_preceeding_whitespace(|this| {
            let span!(first, _) = this.chars.next().unwrap();
            this.lex_identifier(&first)
        });
        if matches!(self.chars.peek(), Some(span!('('))) {
            let span!(lparen_range, _) = self.chars.next().unwrap();
            let (_, parameter_list) = self.lex_delimited(&lparen_range, '(');
            let replacement_list = self.ignore_preceeding_whitespace(|this| {
                this.take_while(|c| c.value != Token::Whitespace(Whitespace::Newline))
                    .collect::<Vec<_>>()
            });
            Some(Spanned::new(
                range.start..macro_name.range.end,
                Token::Directive(Directive::DefineFunction(
                    macro_name,
                    parameter_list,
                    replacement_list,
                )),
            ))
        } else {
            let replacement_list = self.ignore_preceeding_whitespace(|this| {
                this.take_while(|c| c.value != Token::Whitespace(Whitespace::Newline))
                    .collect::<Vec<_>>()
            });
            Some(Spanned::new(
                range.start..macro_name.range.end,
                Token::Directive(Directive::DefineObject(macro_name, replacement_list)),
            ))
        }
    }
    pub fn lex_undefine_direcrive(&mut self, range: &CharLocation) -> Option<Spanned<Token>> {
        let macro_name = self.ignore_preceeding_whitespace(|this| {
            let span!(first, _) = this.chars.next().unwrap();
            this.lex_identifier(&first)
        });
        Some(Spanned::new(
            range.start..macro_name.range.end,
            Token::Directive(Directive::Undefine(macro_name)),
        ))
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
        while let Some(span!(' ' | '\t')) = self.chars.peek() {
            self.next().unwrap();
        }
        f(self)
    }
    pub fn is_equal_within_source(&self, left: &[CharLocation], right: &str) -> bool {
        let left = left
            .iter()
            .flat_map(|range| self.source[range.start..range.end].chars());
        let right = right.chars();

        left.eq(right)
    }
}
