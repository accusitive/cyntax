use std::{collections::HashMap, iter::once, marker::PhantomData, ops::Deref};

use cyntax_lexer::{span, spanned::Spanned, HashedSparseChars, SparseChars, Token};
use radix_trie::Trie;

use crate::tree::{ControlLine, TokenTree};

pub struct ExpandTokens<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> {
    pub source: &'src str,
    pub state: &'state mut HashMap<HashedSparseChars, &'src Vec<&'src Spanned<Token>>>,
    pub token_trees: I
}
impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> Iterator for ExpandTokens<'src, 'state, I> {
    type Item = Vec<&'src Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_trees.next()? {
            // TokenTree::Token(span!(Token::Identifier(identifier))) => {
            //     // if self.state.get(&identifier.hashed(self.source)).is_some() {

            //     // }
            //     todo!()
            // }
            TokenTree::Token(spanned) => {
                return Some(vec![spanned])
            },
            tt @ (TokenTree::IfDef { macro_name, body, opposition } | TokenTree::IfNDef { macro_name, body, opposition } )=> {
                let invert = matches!(tt, TokenTree::IfNDef { .. });

                let key = macro_name.hashed(self.source);
                let mut flag = self.state.get(&key).is_some();
                if invert {
                    flag = !flag;
                }
                if flag {
                    let toks = ExpandTokens {
                        source: self.source,
                        state: self.state,
                        token_trees: body.iter(),
                    }.flatten().collect::<Vec<_>>();
                    return Some(toks);
                } else {
                    let toks = ExpandTokens {
                        source: self.source,
                        state: self.state,
                        token_trees: std::iter::once(opposition.deref()),
                    }.flatten().collect();
                    return Some(toks);
                }

            },
            TokenTree::If { condition, body, opposition } => todo!(),
            TokenTree::Elif { condition, body, opposition } => todo!(),
            TokenTree::Else { body, opposition } => {
                let toks = ExpandTokens {
                    source: self.source,
                    state: self.state,
                    token_trees: body.iter(),
                }.flatten().collect();
                if !matches!(opposition.deref(), TokenTree::Endif) {
                    // TODO: Add an error
                }
                return Some(toks);
            },
            TokenTree::Endif => Some(vec![]),
            TokenTree::Directive(control_line) => {
                match control_line {
                    ControlLine::DefineFunction { macro_name, parameters, replacement_list } => todo!(),
                    ControlLine::DefineObject { macro_name, replacement_list } => {
                        let key = macro_name.hashed(self.source);
                        // TODO error handleing here
                        self.state.insert(key, replacement_list);

                        Some(vec![])
                    },
                    ControlLine::Undefine(sparse_chars) => todo!(),
                    ControlLine::Error(spanned) => todo!(),
                    ControlLine::Warning(spanned) => todo!(),
                    _ => unreachable!()
                }
            },
        }
        // self.token_trees.next()
    }
}
