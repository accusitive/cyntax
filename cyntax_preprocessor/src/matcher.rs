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
            // TokenTree::Token(opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBracket | Punctuator::LeftBrace))) => self.collect_inside(opening_token),
            // TokenTree::OwnedToken(opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBracket | Punctuator::LeftBrace))) => self.collect_inside(&opening_token),
            token => Some(token),
        }
    }
}
impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Matcher<'src, I> {
    pub fn collect_inside(&mut self, opening_token: &Spanned<Token>) -> Option<TokenTree<'src>> {
        let opening_char = opening_token.map_ref(|tok| match tok {
            Token::Punctuator(Punctuator::LeftParen) => '(',
            Token::Punctuator(Punctuator::LeftBracket) => '[',
            Token::Punctuator(Punctuator::LeftBrace) => '{',
            _ => unreachable!(),
        });

        let mut inner = vec![];
        while let Some(token) = self.next() {
            let expected_closer = match opening_char.value {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => unreachable!(),
            };
            let valid_closer = Punctuator::from_char(expected_closer).unwrap();
            match token {
                TokenTree::Token(rp @ span!(Token::Punctuator(punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace)))) if *punc == valid_closer => {
                    return Some(TokenTree::Delimited(opening_char, rp.map_ref(|_| expected_closer), inner));
                }
                TokenTree::OwnedToken(ref rp @ span!(Token::Punctuator(ref punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace)))) if *punc == valid_closer => {
                    return Some(TokenTree::Delimited(opening_char, rp.map_ref(|_| expected_closer), inner));
                }
                _ => {}
            }
            inner.push(token);
        }
        panic!("unmatched delimiter");
    }
}
