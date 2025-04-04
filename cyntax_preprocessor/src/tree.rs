use std::iter::Peekable;

use cyntax_common::{
    ast::{Token, Whitespace},
    spanned::Spanned,
    sparsechars::SparseChars,
};
use cyntax_errors::{Diagnostic, errors::UnterminatedTreeNode};
use cyntax_lexer::span;
pub struct IntoTokenTree<'src, I: Iterator<Item = &'src Spanned<Token>>> {
    pub(crate) source: &'src str,
    pub(crate) tokens: Peekable<I>,
    pub(crate) expecting_opposition: bool,
}
impl<'src, I: Iterator<Item = &'src Spanned<Token>>> Iterator for IntoTokenTree<'src, I> {
    type Item = TokenTree<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.next()?;

        match token {
            span!(Token::ControlLine(inner)) => {
                let control_line = self.parse_control_line(inner);
                match control_line {
                    ControlLine::DefineObject { .. }
                    | ControlLine::DefineFunction { .. }
                    | ControlLine::Error(_)
                    | ControlLine::Warning(_)
                    | ControlLine::Undefine(_) => {
                        return Some(TokenTree::Directive(control_line));
                    }
                    ControlLine::If { condition } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));

                        let opposition = Box::new(self.maybe_opposition());

                        return Some(TokenTree::If {
                            condition,
                            body,
                            opposition,
                        });
                    }

                    ControlLine::IfDef { macro_name } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::IfDef {
                            macro_name,
                            body,
                            opposition,
                        });
                    }
                    ControlLine::IfNDef { macro_name } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::IfNDef {
                            macro_name,
                            body,
                            opposition,
                        });
                    }

                    ControlLine::Elif { .. } | ControlLine::Else if !self.expecting_opposition => {
                        self.unwrap_diagnostic(|_| {
                            Err(cyntax_errors::errors::DanglingEndif(
                                token.range.start..token.range.end,
                            ))
                        })
                    }
                    ControlLine::Elif { condition } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::Elif {
                            condition,
                            body,
                            opposition,
                        });
                    }
                    ControlLine::Else => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());

                        return Some(TokenTree::Else { body, opposition });
                    }
                    // Skip these two, they're inert
                    ControlLine::Empty => self.next(),
                    ControlLine::EndIf => self.unwrap_diagnostic(|_| {
                        Err(cyntax_errors::errors::DanglingEndif(
                            token.range.start..token.range.end,
                        ))
                    }),
                }
            }
            _ => Some(TokenTree::Token(token)),
        }
    }
}
impl<'src, I: Iterator<Item = &'src Spanned<Token>>> IntoTokenTree<'src, I> {
    pub fn until_closer(
        &mut self,
        opener: &Spanned<Token>,
    ) -> Result<Vec<TokenTree<'src>>, UnterminatedTreeNode> {
        let mut body = vec![];

        while let Some(token) = self.tokens.peek() {
            match token {
                span!(Token::ControlLine(control_line)) => {
                    let inner = self.parse_control_line(&control_line);
                    match inner {
                        ControlLine::EndIf | ControlLine::Elif { .. } | ControlLine::Else => {
                            return Ok(body);
                        }
                        ControlLine::Empty => {
                            self.tokens.next().unwrap();
                            continue;
                        }
                        _ => {
                            body.push(self.next().unwrap());
                        }
                    }
                }
                _ => body.push(self.next().unwrap()),
            }
        }
        let error = cyntax_errors::errors::UnterminatedTreeNode {
            opening_token: opener.range.start..opener.range.end,
        };
        Err(error)
    }
    pub fn maybe_opposition(&mut self) -> TokenTree<'src> {
        match self.tokens.peek() {
            Some(span!(Token::ControlLine(control_line))) => {
                let control_line = self.parse_control_line(&control_line);
                match control_line {
                    ControlLine::Elif { .. } | ControlLine::Else => {
                        self.expecting_opposition = true;
                        let tree = self.next().unwrap();
                        self.expecting_opposition = false;
                        return tree;
                    }
                    // skip
                    ControlLine::Empty => self.maybe_opposition(),
                    ControlLine::EndIf => {
                        self.tokens.next().unwrap();
                        return TokenTree::Endif;
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
    // Parse the inside of control line into a usable value
    pub fn parse_control_line(&mut self, tokens: &'src [Spanned<Token>]) -> ControlLine<'src> {
        // Handle empty directive
        if tokens.len() == 0 {
            return ControlLine::Empty;
        }

        let mut tokens_iter = tokens.iter().peekable();
        // utility function to strip all preceeding whitespace
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

        match () {
            _ if Self::is_equal_within_source(self.source, &directive_name, "ifdef") => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self
                    .expect_identifier(&mut tokens_iter)
                    .expect("expected macro_name in ifdef directive");

                return ControlLine::IfDef { macro_name };
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "ifndef") => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self
                    .expect_identifier(&mut tokens_iter)
                    .expect("expected macro_name in ifndef directive");

                return ControlLine::IfNDef { macro_name };
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "else") => {
                return ControlLine::Else;
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "elif") => {
                skip_whitespace(&mut tokens_iter);
                let condition = tokens_iter.collect::<Vec<_>>();
                return ControlLine::Elif { condition };
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "endif") => {
                return ControlLine::EndIf;
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "define") => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self
                    .expect_identifier(&mut tokens_iter)
                    .expect("expected macro_name in ifdef directive");
                // dbg!(&tokens_iter.peek());
                // panic!();
                if matches!(
                    tokens_iter.peek(),
                    Some(span!(Token::Delimited {
                        opener: '(',
                        closer: Some(_),
                        inner_tokens: _
                    }))
                ) {
                    let parameters = tokens_iter.next().unwrap();
                    if matches!(
                        tokens_iter.peek(),
                        Some(span!(Token::Whitespace(Whitespace::Space)))
                    ) {
                        tokens_iter.next().unwrap();
                    }
                    let replacement_list = tokens_iter.collect();
                    return ControlLine::DefineFunction {
                        macro_name,
                        parameters,
                        replacement_list,
                    };
                } else {
                    if matches!(
                        tokens_iter.peek(),
                        Some(span!(Token::Whitespace(Whitespace::Space)))
                    ) {
                        tokens_iter.next().unwrap();
                    }
                    let replacement_list = tokens_iter.collect();
                    return ControlLine::DefineObject {
                        macro_name,
                        replacement_list,
                    };
                }
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "undef") => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self
                    .expect_identifier(&mut tokens_iter)
                    .expect("expected macro_name in ifdef directive");
                return ControlLine::Undefine(macro_name);
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "error") => {
                skip_whitespace(&mut tokens_iter);
                let reason = tokens_iter.next();
                return ControlLine::Error(reason);
            }
            _ if Self::is_equal_within_source(self.source, &directive_name, "warning") => {
                let reason = tokens_iter.next();
                return ControlLine::Warning(reason);
            }
            _ => {
                let directive_range =
                    directive_name.first().unwrap().start..directive_name.last().unwrap().end;
                let err = cyntax_errors::errors::UnknownDirective(directive_range);
                panic!("{}", err.into_why_report().with("", self.source));
            }
        };
    }
    pub fn expect_identifier<'b, I2: Iterator<Item = &'b Spanned<Token>>>(
        &mut self,
        iter: &mut I2,
    ) -> Option<&'b SparseChars> {
        match iter.next()? {
            span!(Token::Identifier(i)) => Some(i),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ControlLine<'src> {
    IfDef {
        macro_name: &'src SparseChars,
    },
    IfNDef {
        macro_name: &'src SparseChars,
    },

    If {
        condition: Vec<&'src Spanned<Token>>,
    },
    Elif {
        condition: Vec<&'src Spanned<Token>>,
    },
    Else,
    EndIf,
    DefineFunction {
        macro_name: &'src SparseChars,
        parameters: &'src Spanned<Token>,
        replacement_list: Vec<&'src Spanned<Token>>,
    },
    DefineObject {
        macro_name: &'src SparseChars,
        replacement_list: Vec<&'src Spanned<Token>>,
    },
    Undefine(&'src SparseChars),
    Error(Option<&'src Spanned<Token>>),
    Warning(Option<&'src Spanned<Token>>),

    Empty,
}
impl<'src, I: Iterator<Item = &'src Spanned<Token>>> IntoTokenTree<'src, I> {
    pub fn is_equal_within_source(source: &'src str, left: &SparseChars, right: &str) -> bool {
        let left = left
            .iter()
            .flat_map(|range| source[range.start..range.end].chars());
        let right = right.chars();

        left.eq(right)
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
#[derive(Debug, Clone)]
pub enum TokenTree<'src> {
    IfDef {
        macro_name: &'src SparseChars,
        body: Vec<TokenTree<'src>>,
        opposition: Box<TokenTree<'src>>,
    },
    IfNDef {
        macro_name: &'src SparseChars,
        body: Vec<TokenTree<'src>>,
        opposition: Box<TokenTree<'src>>,
    },
    If {
        condition: Vec<&'src Spanned<Token>>,
        body: Vec<TokenTree<'src>>,
        opposition: Box<TokenTree<'src>>,
    },
    Elif {
        condition: Vec<&'src Spanned<Token>>,
        body: Vec<TokenTree<'src>>,
        opposition: Box<TokenTree<'src>>,
    },
    Else {
        body: Vec<TokenTree<'src>>,
        /// Must be endif?
        opposition: Box<TokenTree<'src>>,
    },
    // Maybe this shouldn't be a part of the token tree? The only difference would be changing all the opposition fields to an enum like
    // enum Opposition {
    // TokenTree(TokenTree),
    // Endif
    // }
    // Which doesnt seem beneficial in any way
    Endif,
    Token(&'src Spanned<Token>),
    Directive(ControlLine<'src>),
}

