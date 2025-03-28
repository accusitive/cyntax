use std::slice::Iter;

use codespan_reporting::diagnostic::Diagnostic;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    lexer::{HeaderNameKind, PreprocessingToken, Punctuator},
    location::LocationHistory,
};

use super::{PPResult, Preprocessor, TokenStream, L};
type StretchStream<'a> = PeekMoreIterator<Iter<'a, UnstructuredTokenStretch>>;

type DirectiveCondition = Vec<L<PreprocessingToken>>;
type MacroName = L<PreprocessingToken>;

#[derive(Debug, Clone)]
pub enum DirectiveKind {
    DefineObject(MacroName, Vec<L<PreprocessingToken>>),
    DefineFunction(MacroName, Vec<String>, Vec<L<PreprocessingToken>>),
    Undefine(L<PreprocessingToken>),
    Error(Option<L<PreprocessingToken>>),
    Include(L<PreprocessingToken>),

    Ifdef(String),
    IfNdef(String),

    If(DirectiveCondition),
    Elif(DirectiveCondition),

    Else,

    Endif,
}
#[derive(Debug)]
pub enum UnstructuredTokenStretch {
    Tokens(Vec<LocationHistory<PreprocessingToken>>),
    Directive(DirectiveKind),
}
impl Preprocessor {
    pub fn create_token_stretches(&mut self, token_stream: &mut TokenStream) -> PPResult<Vec<UnstructuredTokenStretch>> {
        let mut tg = vec![];
        while let Some(t) = token_stream.peek() {
            match t.value {
                PreprocessingToken::Punctuator(Punctuator::Hash) if t.location.start.col == 0 => {
                    token_stream.next().unwrap();
                    let directive = self.next_non_whitespace_token(token_stream)?;
                    let directive_name = directive.value.as_identifier().unwrap().clone();

                    let directive_tokens = self.collect_until_newline(token_stream);
                    let mut directive_tokens0 = directive_tokens.iter().peekmore();
                    let mut directive_tokens = &mut directive_tokens0;

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
                                    .with_message("Ifndef directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::IfNdef(
                                macro_name.value.as_identifier().unwrap().to_string(),
                            )));
                        }
                        "if" => {
                            directive_tokens.next().unwrap(); // eat whitespace following if

                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::If(directive_tokens.cloned().collect())));
                        }
                        "elif" => {
                            directive_tokens.next().unwrap(); // eat whitespace following elif
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Elif(directive_tokens.cloned().collect())));
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
                                    .with_message("define directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;

                            // Function style
                            if let Some(loc!(PreprocessingToken::Punctuator(Punctuator::LParen))) = directive_tokens.peek() {
                                let _lparen = directive_tokens.next().unwrap(); // consume LPAREN
                                let mut parameters = vec![];
                                while let Some(next_token) = directive_tokens.peek() {
                                    if matches!(next_token.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
                                        break;
                                    }

                                    // dbg!(&parameters, &next_token);
                                    let next = self.next_non_whitespace_token(directive_tokens).unwrap();
                                    if let loc!(PreprocessingToken::Punctuator(Punctuator::DotDotDot)) = next {
                                        // variadic
                                        break;
                                    }
                                    match next.as_identifier() {
                                        Some(identifier) => {
                                            parameters.push(identifier.clone());
                                        }
                                        None => {
                                            return Err(Diagnostic::error()
                                                .with_message("Function-style macro parameters must be identifier")
                                                .with_labels(next.generate_location_labels()))
                                        }
                                    }
                                    if parameters.len() > 0 {
                                        if let Some(loc!(PreprocessingToken::Punctuator(Punctuator::Comma))) = directive_tokens.peek() {
                                            let _comma = directive_tokens.next().unwrap();
                                        }
                                    }
                                }
                                assert!(matches!(directive_tokens.next(), Some(loc!(PreprocessingToken::Punctuator(Punctuator::RParen)))));

                                let replacement_list = directive_tokens.cloned().collect();
                                tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::DefineFunction(macro_name, parameters, replacement_list)));
                            } else {
                                let replacement_list = directive_tokens.cloned().collect();
                                tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::DefineObject(macro_name, replacement_list)));
                            }
                        }
                        "undef" => {
                            let macro_name = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                                Diagnostic::error()
                                    .with_message("undef directive must be followed by an identifier")
                                    .with_labels(directive.generate_location_labels())
                            })?;

                            // Function style
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Undefine(macro_name)));
                        }
                        "error" => {
                            let error_reason = self.next_non_whitespace_token(&mut directive_tokens).ok();

                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Error(error_reason)))
                        }
                        "warning" => {
                            // let error_reason = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                            //     Diagnostic::error()
                            //         .with_message("warning directive must be followed by a reason")
                            //         .with_labels(directive.generate_location_labels())
                            // })?;
                            // tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Error(error_reason)))
                        }
                        "include" => {
                            let header_name = self.next_non_whitespace_token(&mut directive_tokens).map_err(|_| {
                                Diagnostic::error()
                                    .with_message("include directive must be followed by a \"header-name\"/<header-name>")
                                    .with_labels(directive.generate_location_labels())
                            })?;
                            tg.push(UnstructuredTokenStretch::Directive(DirectiveKind::Include(header_name)))
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
/// tuple element refers to the else block

#[derive(Debug)]
pub enum GroupKind {
    IfDef(String, Option<Box<Group>>),
    IfNDef(String, Option<Box<Group>>),
    If(Vec<L<PreprocessingToken>>, Option<Box<Group>>),
    Elif(Vec<L<PreprocessingToken>>, Option<Box<Group>>),

    Else,
    Body,
}
#[derive(Debug)]
pub struct Group {
    pub kind: GroupKind,
    pub content: Vec<GroupChild>,
}
#[derive(Debug)]
pub enum GroupChild {
    Token(LocationHistory<PreprocessingToken>),
    /// May only be DirectiveKind::Define
    Directive(DirectiveKind),
    Group(Group),
}

impl GroupChild {
    pub fn as_token(&self) -> Option<&LocationHistory<PreprocessingToken>> {
        if let Self::Token(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
impl Preprocessor {
    pub fn parse_all_groups(&mut self, stretch_stream: &mut StretchStream) -> Vec<Group> {
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
    pub fn create_conditional_group_kind(&mut self, directive_kind: &DirectiveKind, condition: Vec<L<PreprocessingToken>>, opposition: Option<Box<Group>>) -> GroupKind {
        match directive_kind {
            DirectiveKind::If(_) => GroupKind::If(condition, opposition),
            DirectiveKind::Elif(_) => GroupKind::Elif(condition, opposition),

            x => unreachable!("{:?}", x),
        }
    }
    pub fn create_macro_group_kind(&mut self, directive_kind: &DirectiveKind, macro_name: String, opposition: Option<Box<Group>>) -> GroupKind {
        match directive_kind {
            DirectiveKind::Ifdef(_) => GroupKind::IfDef(macro_name, opposition),
            DirectiveKind::IfNdef(_) => GroupKind::IfNDef(macro_name, opposition),

            _ => unreachable!(),
        }
    }
    pub fn peek_non_whitespace<'long: 'short, 'short>(&'long mut self, stretch_stream: &'long mut StretchStream, n: usize) -> Option<&'short UnstructuredTokenStretch> {
        match stretch_stream.peek_nth(n) {
            tok @ Some(UnstructuredTokenStretch::Tokens(tokens)) => {
                let mut is_all_whitespace = true;
                for token in tokens {
                    if !matches!(token.value, PreprocessingToken::Whitespace(_) | PreprocessingToken::Newline) {
                        is_all_whitespace = false;
                    }
                }
                dbg!(&is_all_whitespace, &tok);
                if is_all_whitespace {
                    return self.peek_non_whitespace(stretch_stream, n + 1);
                } else {
                    return tok.map(|t| &**t);
                }
            }
            Some(tok) => Some(&**tok),
            _ => None,
        }
    }
    pub fn parse_group(&mut self, stretch_stream: &mut StretchStream) -> Option<Group> {
        match stretch_stream.peek() {
            Some(UnstructuredTokenStretch::Tokens(items)) => {
                stretch_stream.next().unwrap();
                let g = Group {
                    kind: GroupKind::Body,
                    content: items.iter().map(|token| GroupChild::Token(token.clone())).collect(),
                };

                Some(g)
            }
            Some(UnstructuredTokenStretch::Directive(directive_kind)) => {
                stretch_stream.next().unwrap();

                match directive_kind {
                    DirectiveKind::Elif(condition) | DirectiveKind::If(condition) => {
                        let mut group_body = vec![];
                        while let Some(stretch) = stretch_stream.peek() {
                            match stretch {
                                UnstructuredTokenStretch::Directive(DirectiveKind::Endif) => {
                                    // self.expect_endif(stretch_stream);
                                    break;
                                }
                                UnstructuredTokenStretch::Directive(DirectiveKind::Else | DirectiveKind::Elif(_)) => break,
                                _ => {}
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }

                        match stretch_stream.peek() {
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Else | DirectiveKind::Elif(_))) => {
                                let opposition = self.parse_group(stretch_stream).unwrap();
                                Some(Group {
                                    kind: self.create_conditional_group_kind(directive_kind, condition.to_vec(), Some(Box::new(opposition))),
                                    content: group_body,
                                })
                            }
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Endif)) => {
                                self.expect_endif(stretch_stream).unwrap();
                                Some(Group {
                                    kind: self.create_conditional_group_kind(directive_kind, condition.to_vec(), None),
                                    content: group_body,
                                })
                            }
                            _ => Some(Group {
                                kind: self.create_conditional_group_kind(directive_kind, condition.to_vec(), None),
                                content: group_body,
                            }),
                        }
                    }
                    DirectiveKind::Ifdef(macro_name) | DirectiveKind::IfNdef(macro_name) => {
                        let mut group_body = vec![];
                        while let Some(stretch) = stretch_stream.peek() {
                            match stretch {
                                UnstructuredTokenStretch::Directive(DirectiveKind::Endif) => {
                                    break;
                                }
                                UnstructuredTokenStretch::Directive(DirectiveKind::Else | DirectiveKind::Elif(_)) => break,
                                _ => {}
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }
                        // dbg!(&macro_name, &stretch_stream);

                        match self.peek_non_whitespace(stretch_stream, 0) {
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Else | DirectiveKind::Elif(_))) => {
                                let opposition = self.parse_group(stretch_stream).unwrap();
                                Some(Group {
                                    kind: self.create_macro_group_kind(directive_kind, macro_name.to_string(), Some(Box::new(opposition))),
                                    content: group_body,
                                })
                            }
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Endif)) => {
                                self.expect_endif(stretch_stream).unwrap();
                                Some(Group {
                                    kind: self.create_macro_group_kind(directive_kind, macro_name.to_string(), None),
                                    content: group_body,
                                })
                            }
                            x => panic!("{:#?}", x),
                        }
                    }
                    DirectiveKind::Else => {
                        let mut group_body = vec![];
                        while let Some(stretch) = stretch_stream.peek() {
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Endif) = stretch {
                                // self.expect_endif(stretch_stream);
                                break;
                            }
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Else) = stretch {
                                panic!("encountered else while looking for closer for else!")
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }

                        match self.peek_non_whitespace(stretch_stream, 0) {
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Else) | UnstructuredTokenStretch::Directive(DirectiveKind::Elif(_))) => {
                                panic!("Cannot have else directive opposing and else directive")
                            }
                            Some(UnstructuredTokenStretch::Directive(DirectiveKind::Endif)) => {
                                self.expect_endif(stretch_stream).unwrap();
                                Some(Group {
                                    kind: GroupKind::Else,
                                    content: group_body,
                                })
                            }
                            x => panic!("{:#?}", x),
                        }
                    }
                    DirectiveKind::DefineObject(_, _)
                    | DirectiveKind::DefineFunction(_, _, _)
                    | DirectiveKind::Undefine(_)
                    | DirectiveKind::Error(_)
                    | DirectiveKind::Include(_) => {
                        let g = Group {
                            kind: GroupKind::Body,
                            content: vec![GroupChild::Directive(directive_kind.clone())],
                        };

                        Some(g)
                    }

                    d => unreachable!("{:#?}", d),
                }
            }
            None => None,
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
            PreprocessingToken::StringLiteral(s) => format!("\"{}\"", s.to_string()),
            PreprocessingToken::Punctuator(punctuator) => punctuator.stringify().to_string(),
            PreprocessingToken::Whitespace(w) => w.to_string(),
            PreprocessingToken::Newline => format!("\n{}", " ".repeat(indent)),
            PreprocessingToken::HeaderName(s, HeaderNameKind::H) => format!("<{}>", s),
            PreprocessingToken::HeaderName(s, HeaderNameKind::Q) => format!("\"{}\"", s),
            token => unimplemented!("token {:?} is not stringifiable yet", &token),
        }
    }
}
