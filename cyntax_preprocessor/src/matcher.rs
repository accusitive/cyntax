use std::fmt::Debug;

use crate::tree::TokenTree;
use cyntax_common::ast::{Punctuator, Token};
use cyntax_common::spanned::Spanned;
use cyntax_lexer::span;
#[derive(Debug)]
pub struct Matcher<'src, I: Debug + Iterator<Item = TokenTree<'src>>> {
    pub inner: I,
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Iterator for Matcher<'src, I> {
    type Item = TokenTree<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next()? {
            TokenTree::Token(lp @ span!(Token::Punctuator(Punctuator::LeftParen))) => {
                let c = lp.map_ref(|tok| match tok {
                    Token::Punctuator(Punctuator::LeftParen) => '(',
                    Token::Punctuator(Punctuator::LeftBracket) => '[',
                    Token::Punctuator(Punctuator::LeftBrace) => '{',
                    _ => unreachable!()
                });

                let mut inner = vec![];
                while let Some(token) = self.next() {
                    match token {
                        TokenTree::Token(rp @ span!(Token::Punctuator(Punctuator::RightParen))) => return Some(TokenTree::Delimited(c, rp.clone().map(|_| ')'), inner)),
                        TokenTree::OwnedToken(rp @ span!(Token::Punctuator(Punctuator::RightParen))) => return Some(TokenTree::Delimited(c, rp.map(|_| ')'), inner)),
                        _ => (),
                    }
                    inner.push(token);
                }
                panic!("unmatched delimiter");
            }
            TokenTree::OwnedToken(ref spanned) => todo!(),
            token => Some(token),
        }
    }
}
