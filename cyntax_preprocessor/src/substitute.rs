use std::{collections::HashMap, fmt::Debug};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_lexer::{lexer::Lexer, span};

use crate::{
    expand::{self, MacroArgument},
    prepend::PrependingPeekableIterator,
};

pub struct ArgumentSubstitutionIterator<I>
where
    I: Debug + Iterator<Item = Spanned<Token>>,
{
    pub replacements: PrependingPeekableIterator<I>,
    pub map: HashMap<String, MacroArgument>,
    pub glue: bool,
    pub glue_string: String,
}

impl<I: Debug + Iterator<Item = Spanned<Token>>> Iterator for ArgumentSubstitutionIterator<I> {
    type Item = Vec<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.replacements.next();
        dbg!(&token);
        let token = token?;

        match &token {
            token if self.glue => {
                Self::stringify_tokens(self.maybe_substitute_arg(token.clone()).iter(), &mut self.glue_string);

                if !matches!(self.replacements.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) {
                    let src = format!("{}", self.glue_string);

                    let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(token.range.clone(), span.value)).collect::<Vec<_>>();

                    self.glue = false;
                    self.glue_string.clear();

                    Some(tokens)
                } else {
                    self.replacements.next().unwrap();
                    Some(vec![])
                }
            }
            token if matches!(self.replacements.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
                self.glue = true;
                Self::stringify_tokens(self.maybe_substitute_arg(token.clone()).iter(), &mut self.glue_string);
                self.replacements.next().unwrap(); // // eat ## 

                Some(vec![])
            }
            // span!(Token::Punctuator(Punctuator::Hash)) => {
            //     let next = self.next().unwrap();
            //     let mut s = String::new();
            //     Self::stringify_tokens(next.iter(), &mut s);

            //     Some(vec![Spanned::new(token.range.clone(), Token::StringLiteral(s))])
            // }

            // lhs if self.glue => {
            //     dbg!(&lhs, &self.replacements.peek());
            //     dbg!(&self.next());
            //     panic!();
            // }
            // token if matches!(self.replacements.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
            // let _hh = self.replacements.next()?;
            // self.replacements.prepend(token.clone());
            // dbg!(&token, &_hh);
            // self.glue = true;
            // Some(vec![])

            // let rhs = self.next()?;
            // let range = lhs.range.start..lhs.range.end;

            // let mut left = String::new();
            // let mut right = String::new();

            // Self::stringify_token(lhs, &mut left);
            // Self::stringify_tokens(rhs.iter(), &mut right);

            // let src = format!("{}{}", left, right);
            // let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(range.clone(), span.value)).collect::<Vec<_>>();

            // Some(tokens)
            // }
            span!(Token::Identifier(identifier)) if self.map.contains_key(identifier) => {
                let expanded = self.map.get(identifier).unwrap().expanded.clone();
                Some(expanded)
            }

            _ => Some(vec![token]),
        }
    }
}

impl<I: Debug + Iterator<Item = Spanned<Token>>> ArgumentSubstitutionIterator<I> {
    pub fn maybe_substitute_arg(&mut self, token: Spanned<Token>) -> Vec<Spanned<Token>> {
        match token {
            span!(Token::Identifier(identifier)) if self.map.contains_key(&identifier) => {
                let expanded = self.map.get(&identifier).unwrap().expanded.clone();
                expanded
            }
            _ => vec![token],
        }
    }
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
