use std::{collections::HashMap, fmt::Debug};

use cyntax_common::{ast::{Punctuator, Token}, spanned::Spanned};

use crate::{expand::MacroArgument, prepend::PrependingPeekableIterator};

pub struct ArgumentSubstitutionIterator<I>
where
    I: Debug + Iterator<Item = Spanned<Token>>,
{
    pub replacements: PrependingPeekableIterator<I>,
    pub map: HashMap<String, MacroArgument>,
    pub stringify: bool,
    pub flue: bool,
}

impl<I: Debug + Iterator<Item = Spanned<Token>>> Iterator for ArgumentSubstitutionIterator<I>
{
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.replacements.next()?;

        match &token.value {
            Token::Punctuator(Punctuator::Hash) => {
                self.stringify = true;
                self.next() // Skip `#` itself, process next token
            }

            Token::Punctuator(Punctuator::HashHash) => {
                // Token pasting (flue)
                self.flue = true;
                self.next() // Skip `##`, handle paste in next call
            }

            Token::Identifier(identifier) if self.map.contains_key(identifier) => {
                let expanded = self.map.get(identifier).unwrap().expanded.clone();
                self.replacements.prepend_extend(expanded.into_iter());
                self.next()
            }

            _ if self.stringify => {
                let mut s = String::new();
                Self::stringify_token(&token, &mut s);
                self.stringify = false;

                Some(Spanned::new(
                    token.range.clone(),
                    Token::StringLiteral(s),
                ))
            }

            _ => Some(token),
        }
    }
}

impl<I: Debug + Iterator<Item = Spanned<Token>>> ArgumentSubstitutionIterator<I> {
    pub fn stringify_tokens<'a, T: Iterator<Item = &'a Spanned<Token>>>(tokens: T, s: &mut String) {
        for token in tokens {
            Self::stringify_token(token, s);
        }
    }
    pub fn stringify_token(token: &Spanned<Token>, s: &mut String) {
        match &token.value {
            Token::Identifier(identifier) => s.push_str(identifier),
            Token::BlueIdentifier(identifier) => s.push_str(identifier),

            Token::StringLiteral(string) => {
                s.push('"');
                s.push_str(string);
                s.push('"');
            }
            Token::PPNumber(number) => {
                s.push_str(number);
            }
            Token::Whitespace(whitespace) => {
                s.push(match whitespace {
                    cyntax_common::ast::Whitespace::Space => ' ',
                    cyntax_common::ast::Whitespace::Newline => '\n',
                    cyntax_common::ast::Whitespace::Tab => '\t',
                });
            }
            Token::Punctuator(punctuator) => s.push_str(&punctuator.to_string()),
            Token::Delimited { opener, closer, inner_tokens } => {
                s.push(opener.value);
                Self::stringify_tokens(inner_tokens.iter(), s);
                s.push(closer.value);
            }
            Token::ControlLine(_) => unreachable!(),
        }
    }
}