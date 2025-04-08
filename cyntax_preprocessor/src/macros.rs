use std::{collections::HashMap, fmt::Debug, time::Instant};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_lexer::{lexer::Lexer, span};

use crate::{
    expand::{Expander, ReplacementList},
    prepend::PrependingPeekableIterator,
    tree::TokenTree,
};

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn handle_function_style_macro_invocation<'a>(&mut self, macro_identifier: &String, macro_name: &Spanned<Token>, parameter_list: &Vec<String>, replacement_list: &Vec<&'src Spanned<Token>>, output: &mut Vec<Spanned<Token>>) {
        dbg!();
        // if the token invocation is failed, ie, the identifier IS a valid function styler macro, but there is no argument list following it
        let next_non_whitespace = self.peek_non_whitespace();
        let is_invocation = matches!(next_non_whitespace, Some(TokenTree::Token(span!(Token::Punctuator(Punctuator::LeftParen))))) || matches!(next_non_whitespace, Some(TokenTree::OwnedToken(span!(Token::Punctuator(Punctuator::LeftParen)))));

        if is_invocation {
            let lparen = self.next_non_whitespace().unwrap().as_token();
            let inside = self.collect_until_closing_delimiter(&lparen, true).unwrap();
            let expanded = self.expand_token_tree(inside, true).unwrap();
            let args = if let span!(Token::Delimited { opener: _, closer: _, inner_tokens }) = expanded.first().as_ref().unwrap() {
                self.split_delimited(inner_tokens.iter())
            } else {
                panic!("this should never be reachable")
            };
            // This should raise an error if this expansion is not the result of a previously expanded macro
            if args.len() != parameter_list.len() {
                output.push(macro_name.clone());
                output.extend(expanded);
                return;
            }

            let map: HashMap<_, _> = parameter_list.into_iter().zip(args).collect();
            let substituted = self.patch_replacement_list(&macro_identifier, replacement_list.to_vec().into_iter().cloned(), &map);

            self.token_trees.prepend_extend(substituted.into_iter());
        } else {
            output.push(macro_name.clone());
            return;
        }
    }
    pub fn handle_object_style_macro_invocaton(&mut self, macro_identifier: &String, replacement_list: &Vec<&'src Spanned<Token>>) {
        dbg!();
        let blue_painted = replacement_list.iter().map(|tok| match *tok {
            // span!(span, Token::Identifier(identifier)) if self.expanding.get(identifier). =>  {
            //     Spanned::new(span.clone(), Token::BlueIdentifier(identifier.to_string()))
            // }
            // span!(span, Token::Identifier(identifier)) if identifier == macro_identifier => {
            //     dbg!(&self.expanding, identifier, macro_identifier);
            //     if *self.expanding.get(identifier).unwrap_or(&false) {
            //         Spanned::new(span.clone(), Token::BlueIdentifier(identifier.to_string()))
            //     } else {
            //         self.expanding.insert(identifier.clone(), true);
            //         Spanned::new(span.clone(), Token::Identifier(identifier.to_string()))
                    
            //     }
            // }
            token => token.clone(),
        });
        self.token_trees.prepend_extend(blue_painted.map(|token| TokenTree::OwnedToken(token).clone()));
    }
    pub fn patch_replacement_list<'a, J: Debug + Iterator<Item = Spanned<Token>>>(&mut self, macro_identifier: &String, replacement_list: J, parameter_to_arg: &HashMap<&String, ReplacementList<'a>>) -> Vec<TokenTree<'src>> {
        let mut replacement_list = PrependingPeekableIterator::new(replacement_list.filter(|tok| !matches!(tok, span!(Token::Whitespace(_)))));
        dbg!();

        let mut output = vec![];
        while let Some(token) = replacement_list.next() {
            match token {
                span!(span, Token::Identifier(identifier)) if &identifier == macro_identifier => {
                    // if *self.expanding.get(&identifier).unwrap_or(&false) {
                    replacement_list.prepend(Spanned::new(span.clone(), Token::BlueIdentifier(identifier)));
                    // } else {
                    // replacement_list.prepend(Spanned::new(span.clone(), Token::Identifier(identifier)));
                    // self.expanding.insert(macro_identifier.clone(), true);
                    // }
                    // replacement_list.prepend(Spanned::new(span.clone(), Token::BlueIdentifier(identifier)));
                }
                span!(Token::Identifier(identifier)) if parameter_to_arg.contains_key(&identifier) => {
                    let replacement = parameter_to_arg.get(&identifier).unwrap();
                    replacement_list.prepend_extend(replacement.to_vec().into_iter().cloned());
                }
                span!(Token::Punctuator(Punctuator::Hash)) => {
                    let param = replacement_list.next().expect("`#` must be followed by macro parameter");
                    if let span!(Token::Identifier(identifier)) = param {
                        if let Some(argument_replacement_list) = parameter_to_arg.get(&identifier) {
                            let mut pasted = String::new();
                            self.stringify_tokens(argument_replacement_list.to_vec().into_iter(), &mut pasted);
                            replacement_list.prepend(Spanned::new(0..0, Token::StringLiteral(pasted)));
                        } else {
                            panic!("no param")
                        }
                    } else {
                        panic!("token following # must be an identifier")
                    }
                }

                token if matches!(replacement_list.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
                    let _hash_hash = replacement_list.next().unwrap();
                    let other = replacement_list.next().unwrap();
                    let range = token.range.start..other.range.end;

                    let mut left = String::new();
                    self.stringify_token(&token, &mut left);
                    let mut right = String::new();
                    dbg!(&other);
                    let expanded_right = self.patch_replacement_list(macro_identifier, std::iter::once(other), parameter_to_arg).into_iter().map(|tt| tt.as_token().into_owned()).collect::<Vec<_>>();
                    dbg!(&expanded_right);
                    self.stringify_tokens(expanded_right.iter(), &mut right);

                    let src = format!("{}{}", left, right);
                    dbg!(&src);
                    let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(range.clone(), span.value)).collect::<Vec<_>>();
                    dbg!(&tokens);
                    replacement_list.prepend_extend(tokens.into_iter());
                }
                _ => {
                    output.push(token);
                }
            }
        }
        output.into_iter().map(|token| TokenTree::OwnedToken(token)).collect()
    }
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
