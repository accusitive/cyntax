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
            TokenTree::Token(lp @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBracket | Punctuator::LeftBrace))) => {
                let c = lp.map_ref(|tok| match tok {
                    Token::Punctuator(Punctuator::LeftParen) => '(',
                    Token::Punctuator(Punctuator::LeftBracket) => '[',
                    Token::Punctuator(Punctuator::LeftBrace) => '{',
                    _ => unreachable!(),
                });

                let mut inner = vec![];
                while let Some(token) = self.next() {
                    let closer = match c.value {
                        '(' => ')',
                        '[' => ']',
                        '{' => '}',
                        _ => unreachable!(),
                    };
                    let valid_closer = Punctuator::from_char(closer).unwrap();
                    match token {
                        TokenTree::Token(rp @ span!(Token::Punctuator(punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace)))) if *punc == valid_closer => {
                            return Some(TokenTree::Delimited(c, rp.map_ref(|_| closer), inner));
                        }
                        TokenTree::OwnedToken(ref rp @ span!(Token::Punctuator(ref punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace)))) if *punc == valid_closer => {
                            return Some(TokenTree::Delimited(c, rp.map_ref(|_| closer), inner));
                        }
                        _ => {
                        },
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
