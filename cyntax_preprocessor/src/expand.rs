use std::{collections::HashMap, iter::once, marker::PhantomData, ops::Deref};

use cyntax_common::{ast::{Punctuator, Token}, spanned::Spanned, sparsechars::{HashedSparseChars, SparseChars}};
use cyntax_lexer::span;

use crate::tree::{ControlLine, TokenTree};

pub struct ExpandTokens<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> {
    pub source: &'src str,
    pub state: &'state mut HashMap<HashedSparseChars, &'src Vec<&'src Spanned<Token>>>,
    pub token_trees: I,
}
impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> Iterator
    for ExpandTokens<'src, 'state, I>
{
    type Item = Vec<&'src Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_trees.next()? {
            TokenTree::Token(s @ span!(Token::Identifier(identifier))) => {
                if let Some(replacement_list) = self.state.get(&identifier.hash(self.source)) {
                    return Some(replacement_list.to_vec());
                } else {
                    return Some(vec![s]);
                }
            }
            TokenTree::Token(spanned) => return Some(vec![spanned]),
            tt @ (TokenTree::IfDef {
                macro_name,
                body,
                opposition,
            }
            | TokenTree::IfNDef {
                macro_name,
                body,
                opposition,
            }) => {
                let invert = matches!(tt, TokenTree::IfNDef { .. });

                let key = macro_name.hash(self.source);
                let mut flag = self.state.get(&key).is_some();
                if invert {
                    flag = !flag;
                }
                if flag {
                    let toks = ExpandTokens {
                        source: self.source,
                        state: self.state,
                        token_trees: body.iter(),
                    }
                    .flatten()
                    .collect::<Vec<_>>();
                    return Some(toks);
                } else {
                    let toks = ExpandTokens {
                        source: self.source,
                        state: self.state,
                        token_trees: std::iter::once(opposition.deref()),
                    }
                    .flatten()
                    .collect();
                    return Some(toks);
                }
            }
            TokenTree::If {
                condition,
                body,
                opposition,
            } => todo!(),
            TokenTree::Elif {
                condition,
                body,
                opposition,
            } => todo!(),
            TokenTree::Else { body, opposition } => {
                let toks = ExpandTokens {
                    source: self.source,
                    state: self.state,
                    token_trees: body.iter(),
                }
                .flatten()
                .collect();
                if !matches!(opposition.deref(), TokenTree::Endif) {
                    // TODO: Add an error
                }
                return Some(toks);
            }
            TokenTree::Endif => Some(vec![]),
            TokenTree::Directive(control_line) => {
                match control_line {
                    ControlLine::DefineFunction {
                        macro_name,
                        parameters,
                        replacement_list,
                    } => {
                        let parameters = self.parse_parameters(parameters);
                        dbg!(&parameters);
                        todo!()
                    }
                    ControlLine::DefineObject {
                        macro_name,
                        replacement_list,
                    } => {
                        let key = macro_name.hash(self.source);
                        // TODO error handleing here
                        self.state.insert(key, replacement_list);

                        Some(vec![])
                    }
                    ControlLine::Undefine(sparse_chars) => todo!(),
                    ControlLine::Error(spanned) => todo!(),
                    ControlLine::Warning(spanned) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        // self.token_trees.next()
    }
}
impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> ExpandTokens<'src, 'state, I> {
    pub fn parse_parameters(&self, delimited: &'src Spanned<Token>) -> Vec<&'src SparseChars>{
        match delimited {
            span!(Token::Delimited('(', ')', inner)) => {
                let mut inner_iter = inner.value.iter();
                let mut parameters = vec![];
                while let Some(token) = inner_iter.next(){
                    if parameters.len() > 0 {
                        self.expect_comma(&mut inner_iter).expect("expected comma");
                    }

                    let identifier = self.enforce_identifier(inner_iter.next().unwrap());
                   
                    parameters.push(identifier);

                }   
                parameters
            }
            _ => unreachable!()
        }
    }
    pub fn enforce_identifier(&self, token: &'src Spanned<Token>) -> &'src SparseChars{
        match token {
            span!(Token::Identifier(inner)) => return inner,
            _ => {
                // TODO: Error
                panic!()
            }
        }
    }
    pub fn expect_comma<J: Iterator<Item = &'src Spanned<Token>>>(&self, iter: &mut J) -> Option<()>{
        match iter.next() {
            Some(span!(Token::Punctuator(Punctuator::Comma))) => Some(()),
            _ => None
        }
    }
}
