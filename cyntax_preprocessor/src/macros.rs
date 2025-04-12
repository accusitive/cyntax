use std::{collections::HashMap, fmt::Debug, time::Instant};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_lexer::{lexer::Lexer, span};

use crate::{
    expand::{ExpandControlFlow, Expander, ReplacementList},
    prepend::PrependingPeekableIterator,
    tree::TokenTree,
};
impl<'src, I: Debug + Iterator<Item = TokenTree>> Expander<'src, I> {
    pub fn parse_parameters<'func>(&mut self, parameter_token: Spanned<Token>) -> Vec<String> {
        if let span!(Token::Delimited { opener, closer, inner_tokens }) = parameter_token {
            let no_whitespace = inner_tokens.iter().filter(|token| !matches!(token, span!(Token::Whitespace(_))));
            let parameters = self.split_delimited(no_whitespace);
            dbg!(&parameters);
            for parameter in &parameters {
                if parameter.len() != 1 {
                    panic!("argument must be exactly one identifier");
                }
            }
            let parameters_as_strings = parameters
                .into_iter()
                .map(|argument| match argument.first().unwrap() {
                    span!(Token::Identifier(identifier)) => identifier,
                    // above is a check to make sure that each parameter is exactly one token, and its just an identifier
                    _ => unreachable!(),
                })
                .cloned()
                .collect();
            parameters_as_strings
        } else {
            unreachable!()
        }
    }
    /// Splits a comma-delimited sequence of tokens into groups.
    ///
    /// - `[]` -> `[]`
    /// - `[,,]` -> `[[], [], []]`
    /// - `[2+5]` -> `[[2+5]]`
    /// - `[2+5,]` -> `[[2+5], []]`
    pub fn split_delimited<'a, L: Iterator<Item = &'a Spanned<Token>>>(&self, mut tokens: L) -> Vec<Vec<&'a Spanned<Token>>> {
        let mut elements = vec![];
        let mut current_element = vec![];

        while let Some(token) = tokens.next() {
            if matches!(token, span!(Token::Punctuator(Punctuator::Comma))) {
                elements.push(std::mem::replace(&mut current_element, Vec::new()));
            } else {
                current_element.push(token);
            }
        }
        elements.push(std::mem::replace(&mut current_element, Vec::new()));

        // manual override for `[]` -> `[]`
        // this can probably be fixed algorithmically
        if elements.len() == 1 && elements.first().unwrap().len() == 0 { vec![] } else { elements }
    }
}
