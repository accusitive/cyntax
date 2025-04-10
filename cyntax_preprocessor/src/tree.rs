use std::{borrow::Cow, iter::Peekable};

use cyntax_common::{
    ast::{Punctuator, Token, Whitespace},
    spanned::Spanned,
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
                    ControlLine::DefineObject { .. } | ControlLine::DefineFunction { .. } | ControlLine::Error(_) | ControlLine::Warning(_) | ControlLine::Undefine(_) => {
                        return Some(TokenTree::Directive(control_line));
                    }
                    ControlLine::If { condition } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));

                        let opposition = Box::new(self.maybe_opposition());

                        return Some(TokenTree::If { condition, body, opposition });
                    }

                    ControlLine::IfDef { macro_name } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::IfDef { macro_name, body, opposition });
                    }
                    ControlLine::IfNDef { macro_name } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::IfNDef { macro_name, body, opposition });
                    }

                    ControlLine::Elif { .. } | ControlLine::Else if !self.expecting_opposition => self.unwrap_diagnostic(|_| Err(cyntax_errors::errors::DanglingEndif(token.range.start..token.range.end))),
                    ControlLine::Elif { condition } => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());
                        return Some(TokenTree::Elif { condition, body, opposition });
                    }
                    ControlLine::Else => {
                        let body = self.unwrap_diagnostic(|this| this.until_closer(token));
                        let opposition = Box::new(self.maybe_opposition());

                        return Some(TokenTree::Else { body, opposition });
                    }
                    // Skip these two, they're inert
                    ControlLine::Empty => self.next(),
                    ControlLine::EndIf => self.unwrap_diagnostic(|_| Err(cyntax_errors::errors::DanglingEndif(token.range.start..token.range.end))),
                }
            }
            _ => Some(TokenTree::LexerToken(token)),
        }
    }
}
impl<'src, I: Iterator<Item = &'src Spanned<Token>>> IntoTokenTree<'src, I> {
    pub fn until_closer(&mut self, opener: &Spanned<Token>) -> Result<Vec<TokenTree<'src>>, UnterminatedTreeNode> {
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

        let directive_name = self.expect_identifier(&mut tokens_iter).expect("expected identifier after directive character");

        match () {
            _ if directive_name == "ifdef" => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self.expect_identifier(&mut tokens_iter).expect("expected macro_name in ifdef directive");

                return ControlLine::IfDef { macro_name };
            }
            _ if directive_name == "ifndef" => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self.expect_identifier(&mut tokens_iter).expect("expected macro_name in ifndef directive");

                return ControlLine::IfNDef { macro_name };
            }
            _ if directive_name == "else" => {
                return ControlLine::Else;
            }
            _ if directive_name == "elif" => {
                skip_whitespace(&mut tokens_iter);
                let condition = tokens_iter.collect::<Vec<_>>();
                return ControlLine::Elif { condition };
            }
            _ if directive_name == "endif" => {
                return ControlLine::EndIf;
            }
            _ if directive_name == "define" => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self.expect_identifier(&mut tokens_iter).expect("expected macro_name in ifdef directive");
                if matches!(tokens_iter.peek(), Some(span!(Token::Punctuator(Punctuator::LeftParen)))) {
                    let opener = tokens_iter.next().unwrap();
                    let mut parameters = vec![];

                    while let Some(token) = tokens_iter.next() {
                        if matches!(token, span!(Token::Punctuator(Punctuator::RightParen))) {
                            let end = parameters.last().map(|param: &Spanned<_>| param.range.end).unwrap_or(opener.range.end);
                            let parameters_token = Token::Delimited {
                                opener: opener.map_ref(|_| '('),
                                closer: token.map_ref(|_| ')'),
                                inner_tokens: parameters,
                            };
                            skip_whitespace(&mut tokens_iter);
                            let replacement_list = tokens_iter.collect();
                            return ControlLine::DefineFunction {
                                macro_name: macro_name,
                                parameters: Spanned::new(opener.range.start..end, parameters_token),
                                replacement_list: replacement_list,
                            };
                        } else {
                            parameters.push(token.clone());
                        }
                    }
                    panic!("todo: error message for unmatched parenthesis in ");
                } else {
                    if matches!(tokens_iter.peek(), Some(span!(Token::Whitespace(Whitespace::Space)))) {
                        tokens_iter.next().unwrap();
                    }
                    skip_whitespace(&mut tokens_iter);

                    let replacement_list = tokens_iter.collect();
                    return ControlLine::DefineObject { macro_name, replacement_list };
                }
            }
            _ if directive_name == "undef" => {
                skip_whitespace(&mut tokens_iter);

                let macro_name = self.expect_identifier(&mut tokens_iter).expect("expected macro_name in ifdef directive");
                return ControlLine::Undefine(macro_name);
            }
            _ if directive_name == "error" => {
                skip_whitespace(&mut tokens_iter);
                let reason = tokens_iter.next();
                return ControlLine::Error(reason);
            }
            _ if directive_name == "warning" => {
                let reason = tokens_iter.next();
                return ControlLine::Warning(reason);
            }
            _ => {
                let directive_range = tokens.first().unwrap().range.start..tokens.last().unwrap().range.end;
                let err = cyntax_errors::errors::UnknownDirective(directive_range);
                panic!("{}", err.into_why_report().with("", self.source));
            }
        };
    }
    pub fn expect_identifier<'b, I2: Iterator<Item = &'b Spanned<Token>>>(&mut self, iter: &mut I2) -> Option<&'b String> {
        match iter.next()? {
            span!(Token::Identifier(i)) => Some(i),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ControlLine<'src> {
    IfDef {
        macro_name: &'src String,
    },
    IfNDef {
        macro_name: &'src String,
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
        macro_name: &'src String,
        parameters: Spanned<Token>,
        replacement_list: Vec<&'src Spanned<Token>>,
    },
    DefineObject {
        macro_name: &'src String,
        replacement_list: Vec<&'src Spanned<Token>>,
    },
    Undefine(&'src String),
    Error(Option<&'src Spanned<Token>>),
    Warning(Option<&'src Spanned<Token>>),

    Empty,
}
impl<'src, I: Iterator<Item = &'src Spanned<Token>>> IntoTokenTree<'src, I> {
    pub fn unwrap_diagnostic<T, E: Diagnostic, F: FnOnce(&mut Self) -> Result<T, E>>(&mut self, value: F) -> T {
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
    Directive(ControlLine<'src>),
    IfDef {
        macro_name: &'src String,
        body: Vec<TokenTree<'src>>,
        opposition: Box<TokenTree<'src>>,
    },
    IfNDef {
        macro_name: &'src String,
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
    Endif,

    LexerToken(&'src Spanned<Token>),
    PreprocessorToken(Spanned<Token>),

    Internal(InternalLeaf)
    
}
#[derive(Debug, Clone)]
pub enum InternalLeaf {
    // Delimited(Spanned<char>, Spanned<char>, Vec<TokenTree<'src>>),
    MacroExpansion(String, Vec<Spanned<Token>>),

    ExpandingMacro(String),
    DoneExpandingMacro(String)
}
impl<'src> TokenTree<'src> {
    pub fn as_cow_token(&self) -> Cow<'src, Spanned<Token>> {
        match self {
            TokenTree::LexerToken(spanned) => Cow::Borrowed(*spanned),
            TokenTree::PreprocessorToken(spanned) => Cow::Owned(spanned.clone()),
            this => panic!("tried to assume {:?} was a token!", this),
        }
    }
    pub fn as_token(&self) ->  Spanned<Token> {
        match self {
            TokenTree::LexerToken(spanned) => (*spanned).clone(),
            TokenTree::PreprocessorToken(spanned) => spanned.clone(),
            this => panic!("tried to assume {:?} was a token!", this),
        }
    }
}
