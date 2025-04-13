use std::{collections::HashMap, fmt::Debug};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_lexer::{lexer::Lexer, span};

use crate::{expand::MacroArgument, prepend::PrependingPeekableIterator};

pub struct ArgumentSubstitutionIterator<I>
where
    I: Debug + Iterator<Item = Spanned<Token>>,
{
    pub replacements: PrependingPeekableIterator<I>,
    pub map: HashMap<String, MacroArgument>,
    pub is_variadic: bool,
    pub variadic_args: Vec<MacroArgument>,
    pub glue_next_token: bool,

    pub glue_string: String,

    pub stringify_next_token: bool,
    pub stringify_string: String,
}

impl<'a, I: Debug + Iterator<Item = Spanned<Token>>> Iterator for ArgumentSubstitutionIterator<I> {
    type Item = Vec<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.replacements.next();
        dbg!(&token);
        let token = token?;

        match &token {
            token if self.glue_next_token => {
                Self::stringify_tokens(self.maybe_substitute_arg(token.clone(), false).iter(), &mut self.glue_string);

                if !matches!(self.replacements.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) {
                    let src = format!("{}", self.glue_string);

                    let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(token.range.clone(), span.value)).collect::<Vec<_>>();

                    self.glue_next_token = false;
                    self.glue_string.clear();
                    Some(tokens)
                } else {
                    self.replacements.next().unwrap();
                    Some(vec![])
                }
            }
            token if self.stringify_next_token => {
                dbg!(&token);
                Self::stringify_tokens(self.maybe_substitute_arg(token.clone(), false).iter(), &mut self.stringify_string);
                self.replacements.prepend(Spanned::new(token.range.clone(), Token::StringLiteral(self.stringify_string.clone())));
                self.stringify_next_token = false;
                self.stringify_string.clear();
                Some(vec![])
            }

            span!(Token::Punctuator(Punctuator::Hash)) => {
                self.stringify_next_token = true;
                Some(vec![])
            }
            token if matches!(self.replacements.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
                self.glue_next_token = true;
                Self::stringify_tokens(self.maybe_substitute_arg(token.clone(), false).iter(), &mut self.glue_string);
                self.replacements.next().unwrap(); // // eat ## 
                Some(vec![])
            }
            span!(Token::Identifier(identifier)) if self.map.contains_key(identifier) => {
                let expanded = self.map.get(identifier).unwrap().expanded.clone();
                Some(expanded)
            }
            span!(Token::Identifier(identifier)) if self.is_variadic && identifier == "__VA_ARGS__" => Some(
                self.variadic_args
                    .iter()
                    .map(|arg| arg.expanded.clone())
                    .flatten()
                    .intersperse(Spanned::new(0..0, Token::Punctuator(Punctuator::Comma)))
                    .collect::<Vec<_>>(),
            ),
            _ => Some(vec![token]),
        }
    }
}

impl<'a, I: Debug + Iterator<Item = Spanned<Token>>> ArgumentSubstitutionIterator<I> {
    pub fn maybe_substitute_arg(&mut self, token: Spanned<Token>, expand: bool) -> Vec<Spanned<Token>> {
        match token {
            span!(Token::Identifier(identifier)) if self.map.contains_key(&identifier) => {
                let expanded = if expand { self.map.get(&identifier).unwrap().expanded.clone() } else { self.map.get(&identifier).unwrap().unexpanded.clone() };
                expanded
            }
            _ => vec![token],
        }
    }
    pub fn stringify_tokens<'b, T: Iterator<Item = &'b Spanned<Token>>>(tokens: T, s: &mut String) {
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
            Token::CharLiteral(chars) => {
                s.push('\'');
                s.push_str(chars);
                s.push('\'');
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
            _ => unreachable!(),
        }
    }
}
