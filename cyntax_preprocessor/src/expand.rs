use std::{collections::HashMap, marker::PhantomData};

use cyntax_lexer::{spanned::Spanned, Token};

use crate::tree::TokenTree;

pub struct ExpandTokens<'a> {
    pub state: &'a mut HashMap<String, usize>,
    pub token_trees: core::slice::Iter<'a, TokenTree<'a>>,
}
impl<'a> Iterator for ExpandTokens<'a> {
    type Item = Vec<&'a Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_trees.next()? {
            TokenTree::Token(spanned) => {
                return Some(vec![spanned])
            },
            TokenTree::IfDef { macro_name, body, opposition } => todo!(),
            TokenTree::IfNDef { macro_name, body, opposition } => todo!(),
            TokenTree::If { condition, body, opposition } => todo!(),
            TokenTree::Elif { condition, body, opposition } => todo!(),
            TokenTree::Else { body, opposition } => todo!(),
            TokenTree::Endif => todo!(),
            TokenTree::Directive(control_line) => todo!(),
        }
        // self.token_trees.next()
    }
}
