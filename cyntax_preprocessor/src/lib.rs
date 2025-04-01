use std::iter::Peekable;

use cyntax_errors::{
    Diagnostic,
    errors::{UnmatchedDelimiter, UnterminatedTreeNode},
};
use cyntax_lexer::{
    Punctuator, SparseChars, Token, Whitespace, lexer::CharLocation, span, spanned::Spanned,
};

pub struct Preprocessor<'a> {
    // macros and whatever
    file_source: &'a str,
    file_name: &'a str,
    tokens: &'a [Spanned<Token>],
}

impl<'a> Preprocessor<'a> {
    pub fn new(
        file_name: &'a str,
        file_source: &'a str,
        tokens: &'a [Spanned<Token>],
    ) -> Preprocessor<'a> {
        Self {
            file_source,
            file_name,
            tokens,
        }
    }
    pub fn create_token_tree(&mut self) {
        let itt: Vec<TokenTree> = IntoTokenTree {
            source: self.file_source,
            tokens: self.tokens.iter().peekable(),
        }
        .collect();
        dbg!(&itt);
    }
}
pub struct IntoTokenTree<'a> {
    source: &'a str,
    tokens: Peekable<core::slice::Iter<'a, Spanned<Token>>>,
}
impl<'a> Iterator for IntoTokenTree<'a> {
    type Item = TokenTree<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.next()?;

        match token {
            span!(Token::ControlLine(inner)) => {
                let control_line = self.parse_control_line(inner);
                dbg!(&control_line);
                match control_line {
                    ControlLine::Empty => self.next(),
                    // This is just used as a marker, so when we actually encounter it in the token stream its just passed over.
                    ControlLine::EndIf => self.next(),
                    ControlLine::IfDef(macro_name) => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        return Some(TokenTree::IfDef {
                            macro_name: macro_name,
                            body,
                            opposition: None,
                        });
                    }
                    n => unimplemented!("{:#?}", n),
                }
            }
            _ => Some(TokenTree::Token(token)),
        }
    }
}
impl<'a> IntoTokenTree<'a> {
    pub fn until_closer(
        &mut self,
        opener: &Spanned<Token>,
    ) -> Result<Vec<TokenTree<'a>>, UnterminatedTreeNode> {
        let mut body = vec![];

        while let Some(token) = self.tokens.peek() {
            match token {
                span!(Token::ControlLine(control_line)) => {
                    let inner = self.parse_control_line(&control_line);
                    if matches!(inner, ControlLine::EndIf) {
                        return Ok(body);
                    }
                    dbg!(&inner);
                }
                _ => body.push(self.next().unwrap()),
            }
        }
        // while let Some(token) = self.tokens.peek() {
        //     // if the next token is a directive
        //     if matches!(token, span!(Token::ControlLine(_))) {
        //         let directive = self.next().unwrap();
        //         if matches!(directive, TokenTree::Endif | TokenTree::Else { .. }) {
        //             return Ok(body);
        //         } else {
        //             body.push(directive)
        //         }
        //     } else {
        //         body.push(TokenTree::Token(self.tokens.next().unwrap()));
        //     }
        // }

        let error = cyntax_errors::errors::UnterminatedTreeNode {
            opening_token: opener.range.start..opener.range.end,
        };
        Err(error)
    }
    pub fn maybe_opposition(&mut self) -> Option<TokenTree<'a>> {
        None
    }
    // Parse the inside of control line into a usable value
    pub fn parse_control_line(&mut self, tokens: &'a [Spanned<Token>]) -> ControlLine<'a> {
        // Handle empty directive
        if tokens.len() == 0 {
            return ControlLine::Empty;
        }
        let mut tokens_iter = tokens.iter().peekable();
        let skip_whitespace = |tokens_iter: &mut Peekable<core::slice::Iter<Spanned<Token>>>| {
            while let Some(token) = tokens_iter.peek() {
                if matches!(token, span!(Token::Whitespace(_))) {
                    tokens_iter.next().unwrap();
                } else {
                    break;
                }
            }
        };

        skip_whitespace(&mut tokens_iter);

        let directive_name = self
            .expect_identifier(&mut tokens_iter)
            .expect("expected identifier after directive character");

        // return
        match () {
            _ if Self::is_equal_within_source(self.source, &directive_name, "ifdef") => {
           
                skip_whitespace(&mut tokens_iter);
           
                let macro_name = self
                    .expect_identifier(&mut tokens_iter)
                    .expect("expected macro_name in ifdef directive");
           
                return ControlLine::IfDef(macro_name);
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "endif") => {
                return ControlLine::EndIf;
            }
            _ => unimplemented!(),
        };
    }
    pub fn expect_identifier<'b, I: Iterator<Item = &'b Spanned<Token>>>(
        &mut self,
        iter: &mut I,
    ) -> Option<&'b SparseChars> {
        match iter.next()? {
            span!(Token::Identifier(i)) => Some(i),
            _ => None,
        }
    }
}
#[derive(Debug)]
pub enum ControlLine<'a> {
    Empty,
    IfDef(&'a SparseChars),
    EndIf,
}
impl<'a> IntoTokenTree<'a> {
    pub fn is_equal_within_source(source: &'a str, left: &[CharLocation], right: &str) -> bool {
        let left = left
            .iter()
            .flat_map(|range| source[range.start..range.end].chars());
        let right = right.chars();

        left.eq(right)
    }
    pub fn ignore_preceeding_whitespace<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        while let Some(span!(Token::Whitespace(Whitespace::Space | Whitespace::Tab))) =
            self.tokens.peek()
        {
            self.tokens.next().unwrap();
        }
        f(self)
    }
    pub fn unwrap_diagnostic<T, E: Diagnostic, F: FnOnce(&mut Self) -> Result<T, E>>(
        &mut self,
        value: F,
    ) -> T {
        match value(self) {
            Ok(value) => value,
            Err(e) => {
                let why = e.into_why_report();
                panic!("{}", why.with("", self.source));
            }
        }
    }
}
#[derive(Debug)]
pub enum TokenTree<'a> {
    IfDef {
        macro_name: &'a SparseChars,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>,
    },
    IfNDef {
        macro_name: &'a Spanned<Token>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>,
    },
    If {
        condition: Vec<Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>,
    },
    ElseIf {
        condition: Vec<Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>,
    },
    Else {
        body: Vec<TokenTree<'a>>,
    },
    Endif,
    Token(&'a Spanned<Token>),
}
