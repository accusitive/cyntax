use std::{ops::Deref, slice::Iter};

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
// #[derive(Debug)]
// pub struct TranslationUnit {
//     groups: Vec<GroupChild>,
// }

// #[derive(Debug, Clone)]
// pub struct Group {
//     kind: GroupKind,
//     children: Vec<GroupChild>,
// }

// #[derive(Debug, Clone)]
// pub enum GroupChild {
//     Group(Box<Group>),
//     Directive,
//     Endif,
//     Token(LocationHistory<PreprocessingToken>),
// }

// #[derive(Debug, Clone)]
// pub enum GroupKind {
//     If(IfGroupKind),
//     Else(),
//     Elif,
//     /// Global group, in a file with no directives, every token is in this group
//     Global,
// }

// #[derive(Debug, Clone)]
// pub enum IfGroupKind {
//     Ifdef,
//     Ifndef,
//     If(LocationHistory<Expression>),
// }
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

    // pub fn pp_token_to_token<'a, T: IntoIterator<Item = &'a LocationHistory<PreprocessingToken>>>(&self, token_stream: T) -> Vec<LocationHistory<Token>> {
    //     let mut tokens = vec![];

    //     for token in token_stream {
    //         tokens.push(token.same(match &token.value {
    //             PreprocessingToken::Identifier(i) if Keyword::from_str(&i).is_some() => Token::Keyword(Keyword::from_str(&i).unwrap()),
    //             PreprocessingToken::Identifier(identifier) => Token::Identifier(identifier.to_owned()),
    //             PreprocessingToken::Number(n) if !n.contains(".") => {
    //                 Token::Constant(crate::preprocess::ast::Constant::Integer(IntConstant::parse(&token.same(n.to_owned())).unwrap()))
    //             }
    //             PreprocessingToken::Punctuator(punctuator) => Token::Punctuator(punctuator.to_owned()),

    //             PreprocessingToken::CharacterConstant => todo!(),
    //             PreprocessingToken::StringLiteral(_) => todo!(),
    //             PreprocessingToken::Error(_) => todo!(),
    //             PreprocessingToken::Newline | PreprocessingToken::Whitespace(_) => continue,
    //             x => unreachable!("x{:?} is unreachable", x),
    //         }));
    //     }
    //     tokens
    // }
    // pub fn s(&self, t: &TranslationUnit) -> String {
    //     let mut s = String::new();
    //     for group in &t.groups {
    //         self.s_g(&mut s, group, 0);
    //     }
    //     s
    // }

    // pub fn s_g(&self, s: &mut String, g: &GroupChild, indent: usize) {
    //     let indent_str = "|".to_string() + &(" ".repeat(indent));
    //     match g {
    //         GroupChild::Group(group) => {
    //             s.push_str(&indent_str);
    //             match &group.kind {
    //                 GroupKind::If(IfGroupKind::If(_)) => s.push_str("#if"),
    //                 GroupKind::If(IfGroupKind::Ifdef) => s.push_str("#ifdef"),
    //                 GroupKind::If(IfGroupKind::Ifndef) => s.push_str("#ifndef"),
    //                 GroupKind::Else() => s.push_str("#else"),
    //                 GroupKind::Elif => s.push_str("#elif"),
    //                 GroupKind::Global => {},
    //                                 }
    //             s.push('\n');
    //             for child in &group.children {
    //                 self.s_g(s, child, indent + 2);
    //             }
    //         }
    //         GroupChild::Directive => {
    //             s.push_str("#");
    //         }
    //         GroupChild::Endif => {
    //             s.push_str(&indent_str);
    //             s.push_str("#endif\n");
    //         }
    //         GroupChild::Token(location_history) => {
    //             s.push_str(&indent_str);
    //             s.push_str(&self.stringify_token(location_history));
    //             s.push('\n');
    //         }
    //     }
    // }

    // pub fn stringify_token(&self, token: &LocationHistory<PreprocessingToken>) -> String {
    //     match &token.value {
    //         PreprocessingToken::Identifier(i) => i.to_string(),
    //         PreprocessingToken::Number(num) => num.to_string(),
    //         PreprocessingToken::StringLiteral(s) => s.to_string(),
    //         PreprocessingToken::Punctuator(punctuator) => punctuator.stringify().to_string(),
    //         PreprocessingToken::Whitespace(w) => w.to_string(),
    //         PreprocessingToken::Newline => "".to_string(),
    //         token => unimplemented!("token {:?} is not stringifiable yet", &token),
    //     }
    // }
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
                            if let UnstructuredTokenStretch::Directive(DirectiveKind::Endif) = stretch {
                                self.expect_endif(stretch_stream);
                                break;
                            }
                            let inner = self.parse_group(stretch_stream);
                            let inner_group = inner.map(|g| vec![GroupChild::Group(g)]).unwrap_or(vec![]);
                            group_body.extend(inner_group);
                        }

                        Some(Group {
                            kind: GroupKind::Ifdef,
                            content: group_body,
                        })
                    }
                    DirectiveKind::IfNdef(_) => todo!(),
                    DirectiveKind::Endif => todo!(),
                    DirectiveKind::Else => todo!(),
                    DirectiveKind::DefineObject(location_history) => todo!(),
                    DirectiveKind::DefineFunction(location_history) => todo!(),
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
}
// impl PP {
//     pub fn parse_global(&mut self, stretch_stream: &mut StretchStream) -> PPR<Group> {
//         let mut group = Group {
//             kind: GroupKind::Global,
//             content: vec![],
//         };
//         self.parse_group(&mut group, stretch_stream)?;
//         Ok(group)
//     }
//    pub fn parse_group(&mut self, group: &mut Group, stretch_stream: &mut StretchStream) -> PPR<()> {

//         match &stretch_stream.peek() {
//             Some(UnstructuredTokenStretch::Tokens(items)) => {
//                 stretch_stream.next().unwrap();

//                 group.content.extend(items.iter().map(|item| GroupChild::Token(item.clone())));
//                 println!("extended");
//             },
//             Some(UnstructuredTokenStretch::Directive(directive_kind)) => {
//                 match directive_kind {
//                     DirectiveKind::Ifdef(i) => {
//                         while let
//                     },
//                     _ => {}
//                 }
//                 // stretch_stream.next().unwrap();
//                 // let mut g = Group {
//                 //     kind: GroupKind::Ifdef,
//                 //     content: vec![],
//                 // };
//                 // println!("recursing");
//                 // self.parse_group(&mut g, stretch_stream)?;
//             },
//             None => {}
//         }
//         Ok(())
//     }
// }
