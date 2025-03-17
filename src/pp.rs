use std::{fmt::Write, ops::Deref, slice::Iter};

use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    lexer::{PreprocessingToken, Punctuator},
    location::{Located, LocationHistory},
    parser::{
        self,
        ast::{Expression, Keyword},
        Parser,
    },
    preprocess::ast::{IntConstant, Token},
};
type PPR<T> = Result<T, Diagnostic<usize>>;
type TokenStream<'a> = PeekMoreIterator<Iter<'a, LocationHistory<PreprocessingToken>>>;
type StretchStream<'a> = PeekMoreIterator<Iter<'a, UnstructuredTokenStretch>>;

type L<T> = LocationHistory<T>;

macro_rules! loc {
    ($p: pat) => {
        L { value: $p, .. }
    };
}
#[derive(Debug)]
pub struct PP {
    // groups: Vec<Group>,
    // current_group: Vec<usize>, // current_group: Group,
}
#[derive(Debug)]
pub enum DirectiveKind {
    Ifdef(String),
    IfNdef(String),
    Endif,
    Else,
    ElseIf,
    DefineObject(L<PreprocessingToken>),
    DefineFunction(L<PreprocessingToken>),
}
#[derive(Debug)]
pub enum UnstructuredTokenStretch {
    Tokens(Vec<LocationHistory<PreprocessingToken>>),
    Directive(DirectiveKind),
}
impl PP {
    pub fn new() -> Self {
        Self {}
    }
    pub fn next_non_whitespace_token<'a: 'b, 'b>(&mut self, token_stream: &'a mut TokenStream) -> PPR<LocationHistory<PreprocessingToken>> {
        match token_stream.next() {
            Some(loc!(PreprocessingToken::Whitespace(_))) => self.next_non_whitespace_token(token_stream),
            Some(t) => Ok(t.clone()),
            None => Err(Diagnostic::bug().with_message("Unexpected EOF")),
        }
    }
    pub fn collect_until_newline(&mut self, token_stream: &mut TokenStream) -> Vec<LocationHistory<PreprocessingToken>> {
        let mut tokens = vec![];
        while let Some(t) = token_stream.peek() {
            if matches!(t.value, PreprocessingToken::Newline) {
                break;
            } else {
                tokens.push(token_stream.next().unwrap().clone());
            }
        }

        tokens
    }
    pub fn groups_tokens(&mut self, token_stream: &mut TokenStream) -> PPR<Vec<UnstructuredTokenStretch>> {
        let mut tg = vec![];
        while let Some(t) = token_stream.peek() {
            match t.value {
                PreprocessingToken::Punctuator(Punctuator::Hash) if t.location.start.col == 0 => {
                    token_stream.next().unwrap();
                    let directive = self.next_non_whitespace_token(token_stream)?;
                    let directive_name = directive.value.as_identifier().unwrap().clone();

                    let directive_tokens = self.collect_until_newline(token_stream);
                    let mut directive_tokens = directive_tokens.iter().peekmore();

                    match directive_name.as_str() {
                        "ifdef" => {
                            let macro_name = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                                Diagnostic::error()
                                    .with_message("Ifdef directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Ifdef(
                                macro_name.value.as_identifier().unwrap().to_string(),
                            )));
                        }
                        "ifndef" => {
                            let macro_name = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                                Diagnostic::error()
                                    .with_message("Ifdef directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::IfNdef(
                                macro_name.value.as_identifier().unwrap().to_string(),
                            )));
                        }
                        "else" => {
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Else));
                        }
                        "endif" => {
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Endif));
                        }
                        "define" => {
                            let macro_name = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                                Diagnostic::error()
                                    .with_message("Ifdef directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;

                            // Function style
                            if let Some(loc!(PreprocessingToken::Punctuator(Punctuator::LParen))) = token_stream.peek() {
                                tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::DefineFunction(macro_name)));
                            } else {
                                tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::DefineObject(macro_name)));
                            }
                        }
                        other => unimplemented!(" {other:?} is unimplemented"),
                    }
                }
                _ => {
                    let next = token_stream.next().unwrap().clone();
                    match tg.last_mut() {
                        Some(UnstructuredTokenStretch::Tokens(tokens)) => tokens.push(next),
                        _ => tg.push(UnstructuredTokenStretch::Tokens(vec![next])),
                    }
                }
            }
        }
        Ok(tg)
    }
}
#[derive(Debug)]
pub enum GroupKind {
    Ifdef,
    Else,
    Elif,
    Global,
}
#[derive(Debug)]
pub struct Group {
    kind: GroupKind,
    content: Vec<GroupChild>,
}
#[derive(Debug)]
pub enum GroupChild {
    Token(LocationHistory<PreprocessingToken>),
    Group(Group),
}
impl PP {
    pub fn pg(&mut self, stretch_stream: &mut StretchStream) -> Vec<Group> {
        let mut groups = vec![];
        while let Some(_) = stretch_stream.peek() {
            let g = self.parse_group(stretch_stream);
            if let Some(g) = g {
                groups.push(g);
            } else {
                break;
            }
        }

        groups
    }
    pub fn parse_group(&mut self, stretch_stream: &mut StretchStream) -> Option<Group> {
        match stretch_stream.peek().unwrap() {
            UnstructuredTokenStretch::Tokens(items) => {
                stretch_stream.next().unwrap();
                let g = Group {
                    kind: GroupKind::Global,
                    content: items.iter().map(|token| GroupChild::Token(token.clone())).collect(),
                };

                Some(g)
            }
            UnstructuredTokenStretch::Directive(directive_kind) => {
                stretch_stream.next().unwrap();

                match directive_kind {
                    DirectiveKind::Ifdef(_) => {
                        // self.expect_endif(stretch_stream).unwrap();
                        let mut group_body = vec![];
                        while let Some(stretch) = stretch_stream.peek() {
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Endif | DirectiveKind::Else) = stretch {
                                self.expect_endif(stretch_stream);
                                break;
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }

                        let opposition = self.parse_group(stretch_stream).unwrap();
                        dbg!(&opposition);

                        Some(Group {
                            kind: GroupKind::Ifdef,
                            content: group_body,
                        })
                    }
                    DirectiveKind::Else => {
                        let mut group_body = vec![];
                        while let Some(stretch) = stretch_stream.peek() {
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Endif) = stretch {
                                self.expect_endif(stretch_stream);
                                break;
                            }
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Else) = stretch {
                                panic!("encountered else while looking for closer for else!")
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }

                        Some(Group {
                            kind: GroupKind::Else,
                            content: group_body,
                        })
                    }
                    x => todo!("todo: implement {:#?} directive", x),
                }
            }
        }
    }

    pub fn expect_endif(&mut self, stretch_stream: &mut StretchStream) -> Option<()> {
        if let Some(UnstructuredTokenStretch::Directive(DirectiveKind::Endif)) = stretch_stream.peek() {
            stretch_stream.next().unwrap();
            return Some(());
        } else {
            return None;
        }
    }
    pub fn stringify_token(&self, indent: usize, token: &LocationHistory<PreprocessingToken>) -> String {
        match &token.value {
            PreprocessingToken::Identifier(i) => i.to_string(),

            PreprocessingToken::Number(num) => num.to_string(),

            PreprocessingToken::StringLiteral(s) => s.to_string(),

            PreprocessingToken::Punctuator(punctuator) => punctuator.stringify().to_string(),

            PreprocessingToken::Whitespace(w) => w.to_string(),

            PreprocessingToken::Newline => format!("\n{}", " ".repeat(indent)),

            token => unimplemented!("token {:?} is not stringifiable yet", &token),
        }
    }
    pub fn print_group(&mut self, s: &mut String, indent: usize, g: &Group) {
        let mut is_global = false;
        match g.kind {
            GroupKind::Ifdef => s.write_str("#ifdef"),
            GroupKind::Else => s.write_str("#else"),
            GroupKind::Elif => s.write_str("#elif"),
            GroupKind::Global => {
                is_global = true;
                s.write_str("")
            }
        }
        .unwrap();

        for content in &g.content {
            let i = if is_global { 0 } else { 2 };
            self.print_group_child(s, indent + i, content);
        }
    }
    pub fn print_group_child(&mut self, s: &mut String, indent: usize, gc: &GroupChild) {
        match gc {
            GroupChild::Token(token) => {
                s.push_str(&self.stringify_token(indent, token));
            }
            GroupChild::Group(group) => self.print_group(s, indent , group),
        }
    }
}
