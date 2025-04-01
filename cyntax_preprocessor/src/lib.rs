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
                dbg!("&a");
                let control_line = self.parse_control_line(inner);
                match control_line {
                    ControlLine::DefineObject { .. }
                    | ControlLine::DefineFunction { .. }
                    | ControlLine::Error(_)
                    | ControlLine::Warning(_)
                    | ControlLine::Undefine(_) => {
                        return Some(TokenTree::Token(token));
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
                    ControlLine::EndIf => self.next(),
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
    pub fn maybe_opposition(&mut self) -> TokenTree<'a> {
        match self.tokens.peek() {
            Some(span!(Token::ControlLine(control_line))) => {
                let control_line = self.parse_control_line(&control_line);
                match control_line {
                    ControlLine::Elif { .. } | ControlLine::Else => {
                        let tree = self.next().unwrap();
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
    pub fn parse_control_line(&mut self, tokens: &'a [Spanned<Token>]) -> ControlLine<'a> {
        // Handle empty directive
        dbg!(&tokens);
        if tokens.len() == 0 {
            return ControlLine::Empty;
        }
        dbg!(&"ASDASD");
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
                if matches!(
                    tokens_iter.peek(),
                    Some(span!(Token::Delimited('(', close, inner)))
                ) {
                    let parameters = tokens_iter.next().unwrap();
                    let replacement_list = tokens_iter.collect();
                    return ControlLine::DefineFunction {
                        macro_name,
                        parameters,
                        replacement_list,
                    };
                } else {
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
                return ControlLine::Error(reason);
            }
            _ => {
                let directive_range =
                    directive_name.first().unwrap().start..directive_name.last().unwrap().end;
                let err = cyntax_errors::errors::UnknownDirective(directive_range);
                panic!("{}", err.into_why_report().with("", self.source));
            }
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
    IfDef {
        macro_name: &'a SparseChars,
    },
    IfNDef {
        macro_name: &'a SparseChars,
    },

    If {
        condition: Vec<&'a Spanned<Token>>,
    },
    Elif {
        condition: Vec<&'a Spanned<Token>>,
    },
    Else,
    EndIf,
    DefineFunction {
        macro_name: &'a SparseChars,
        parameters: &'a Spanned<Token>,
        replacement_list: Vec<&'a Spanned<Token>>,
    },
    DefineObject {
        macro_name: &'a SparseChars,
        replacement_list: Vec<&'a Spanned<Token>>,
    },
    Undefine(&'a SparseChars),
    Error(Option<&'a Spanned<Token>>),
    Warning(Option<&'a Spanned<Token>>),

    Empty,
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
        opposition: Box<TokenTree<'a>>,
    },
    IfNDef {
        macro_name: &'a SparseChars,
        body: Vec<TokenTree<'a>>,
        opposition: Box<TokenTree<'a>>,
    },
    If {
        condition: Vec<&'a Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Box<TokenTree<'a>>,
    },
    ElseIf {
        condition: Vec<Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Box<TokenTree<'a>>,
    },
    Elif {
        condition: Vec<&'a Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Box<TokenTree<'a>>,
    },
    Else {
        body: Vec<TokenTree<'a>>,
        /// Must be endif?
        opposition: Box<TokenTree<'a>>,
    },
    // Maybe this shouldn't be a part of the token tree? The only difference would be changing all the opposition fields to an enum like
    // enum Opposition {
    // TokenTree(TokenTree),
    // Endif
    // }
    // Which doesnt seem beneficial in any way
    Endif,
    Token(&'a Spanned<Token>),
}
