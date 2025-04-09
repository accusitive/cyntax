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
pub(crate) enum ExpandFunctionMacroControlFlow<'src> {
    Final(Vec<Spanned<Token>>),
    Prepend(Vec<TokenTree<'src>>),
}
impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn handle_function_style_macro_invocation<'a>(&mut self, macro_identifier: &String, macro_name: &Spanned<Token>, parameter_list: &Vec<String>, replacement_list: &Vec<&'src Spanned<Token>>) -> TokenTree<'src> {
        dbg!(macro_identifier);
        // if the token invocation is failed, ie, the identifier IS a valid function styler macro, but there is no argument list following it
        let next_non_whitespace = self.peek_non_whitespace();
        let is_invocation = matches!(next_non_whitespace, Some(TokenTree::Token(span!(Token::Delimited { .. } | Token::Punctuator(Punctuator::LeftParen)))))
                               || matches!(next_non_whitespace, Some(TokenTree::OwnedToken(span!(Token::Delimited { .. } | Token::Punctuator(Punctuator::LeftParen)))));
        dbg!(&is_invocation);
        if is_invocation {
            let expanded = match next_non_whitespace {
                Some(TokenTree::OwnedToken(ref d@ span!(Token::Delimited { .. }))) => {self.next_non_whitespace().unwrap(); vec![d.clone()]},
                Some(TokenTree::Token(d@ span!(Token::Delimited { .. }))) => {self.next_non_whitespace().unwrap(); vec![d.clone()]}
                _ => {
                    let lparen = self.next_non_whitespace().unwrap().as_token();
                    let inside = self.collect_until_closing_delimiter(&lparen, true).unwrap();
                    let expanded = self.fully_expand_token_tree(inside, true);
                    expanded
                }
            };

            let args = if let span!(Token::Delimited { opener: _, closer: _, inner_tokens }) = expanded.first().as_ref().unwrap() {
                self.split_delimited(inner_tokens.iter())
            } else {
                panic!("this should never be reachable")
            };
            dbg!(&args);
            // This should raise an error if this expansion is not the result of a previously expanded macro
            if args.len() != parameter_list.len() {
                let mut f = vec![macro_name.clone()];
                f.extend(expanded);

                return TokenTree::MacroExpansion(macro_identifier.clone(), f);
            }

            let map: HashMap<_, _> = parameter_list.into_iter().zip(args).collect();
            dbg!(&map);
            let substituted = self.patch_replacement_list(&macro_identifier, replacement_list.to_vec().into_iter().cloned(), &map);

            return TokenTree::MacroExpansion(macro_identifier.clone(), substituted);
        } else {
            return TokenTree::MacroExpansion(macro_identifier.clone(), vec![macro_name.clone()]);
            // return TokenTree::OwnedToken(Spanned::new(macro_name.range.clone(), Token::BlueIdentifier(macro_identifier.clone())));
        }
    }
    pub fn handle_object_style_macro_invocaton(&mut self, macro_identifier: &String, replacement_list: &Vec<&'src Spanned<Token>>) -> TokenTree<'src> {
        dbg!();
        let patched = self.patch_replacement_list(macro_identifier, replacement_list.to_vec().into_iter().cloned(), &HashMap::new());
        // let blue_painted = replacement_list.iter().map(|tok| match *tok {
        //     token => token.clone(),
        // });
        TokenTree::MacroExpansion(macro_identifier.clone(), patched)

        // self.token_trees.prepend_extend(blue_painted);
    }
    pub fn patch_replacement_list<'a, J: Debug + Iterator<Item = Spanned<Token>>>(&mut self, macro_identifier: &String, replacement_list: J, parameter_to_arg: &HashMap<&String, ReplacementList<'a>>) -> Vec<Spanned<Token>> {
        let mut replacement_list = PrependingPeekableIterator::new(replacement_list.filter(|tok| !matches!(tok, span!(Token::Whitespace(_)))));
        dbg!(&replacement_list);

        let mut left = String::new();
        let mut right = String::new();

        let substitute_arg = |token: Spanned<Token>| match token {
            span!(Token::Identifier(identifier)) if parameter_to_arg.contains_key(&identifier) => parameter_to_arg.get(&identifier).unwrap().to_vec().into_iter().cloned().collect(),
            _ => vec![token],
        };

        let mut output = vec![];
        while let Some(token) = replacement_list.next() {
            dbg!(&token,);
            match token {
                span!(Token::Identifier(ref identifier)) if parameter_to_arg.contains_key(identifier) => {
                    replacement_list.prepend_extend(substitute_arg(token).into_iter());
                }
                span!(Token::Punctuator(Punctuator::Hash)) => {
                    let next = replacement_list.next().unwrap();
                    let mut s = String::new();
                    self.stringify_token(&next, &mut s);
                    output.push(Spanned::new(token.range.clone(), Token::StringLiteral(s)));
                }
                lhs if matches!(replacement_list.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
                    let hh = replacement_list.next().unwrap();
                    let rhs = replacement_list.next().unwrap();
                    let range = lhs.range.start..rhs.range.end;

                    left.clear();
                    right.clear();

                    let left_expanded = substitute_arg(lhs);
                    let right_expanded = substitute_arg(rhs);

                    self.stringify_tokens(left_expanded.iter(), &mut left);
                    self.stringify_tokens(right_expanded.iter(), &mut right);

                    let src = format!("{}{}", left, right);
                    let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(range.clone(), span.value)).collect::<Vec<_>>();

                    output.extend(tokens);
                }
                token => {
                    output.push(token);
                }
            }

            // match token {
            //     span!(Token::Identifier(identifier)) if parameter_to_arg.contains_key(&identifier) && !matches!(replacement_list.peek(), Some(span!(Token::Punctuator(Punctuator::Hash | Punctuator::HashHash)))) => {
            //         let replacement = parameter_to_arg.get(&identifier).unwrap();
            //         replacement_list.prepend_extend(replacement.to_vec().into_iter().cloned());
            //     }
            //     span!(span, Token::Identifier(identifier)) if self.macros.contains_key(&identifier) => {
            //         output.push(Spanned::new(span.clone(), Token::Identifier(identifier)));
            //     }
            //     span!(Token::Punctuator(Punctuator::Hash)) => {
            //         let param = replacement_list.next().expect("`#` must be followed by macro parameter");
            //         if let span!(Token::Identifier(identifier)) = param {
            //             if let Some(argument_replacement_list) = parameter_to_arg.get(&identifier) {
            //                 let mut pasted = String::new();
            //                 self.stringify_tokens(argument_replacement_list.to_vec().into_iter(), &mut pasted);
            //                 replacement_list.prepend(Spanned::new(0..0, Token::StringLiteral(pasted)));
            //             } else {
            //                 panic!("no param")
            //             }
            //         } else {
            //             panic!("token following # must be an identifier")
            //         }
            //     }

            //     token if matches!(replacement_list.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
            //         let _hash_hash = replacement_list.next().unwrap();
            //         dbg!(&_hash_hash);
            //         let other = replacement_list.next().unwrap();
            //         let range = token.range.start..other.range.end;

            //         let mut left = String::new();
            //         let expanded_left = self.patch_replacement_list(macro_identifier, std::iter::once(token), parameter_to_arg).into_iter().map(|tt| tt).collect::<Vec<_>>();

            //         self.stringify_tokens(expanded_left.iter(), &mut left);
            //         let mut right = String::new();
            //         dbg!(&other);
            //         let expanded_right = self.patch_replacement_list(macro_identifier, std::iter::once(other), parameter_to_arg).into_iter().map(|tt| tt).collect::<Vec<_>>();
            //         dbg!(&expanded_right);
            //         self.stringify_tokens(expanded_right.iter(), &mut right);

            //         let src = format!("{}{}", left, right);
            //         dbg!(&src);
            //         let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(range.clone(), span.value)).collect::<Vec<_>>();
            //         dbg!(&tokens);
            //         replacement_list.prepend_extend(tokens.into_iter());
            //     }
            //     _ => {
            //         output.push(token);
            //     }
            // }
        }
        output.into_iter().map(|token| token).collect()
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
