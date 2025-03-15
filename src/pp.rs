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
type TokenSubset<'a> = PeekMoreIterator<Iter<'a, LocationHistory<PreprocessingToken>>>;

#[derive(Debug)]
pub struct TranslationUnit {
    groups: Vec<GroupChild>,
}

#[derive(Debug, Clone)]
pub struct Group {
    kind: GroupKind,
    children: Vec<GroupChild>,
}

#[derive(Debug, Clone)]
pub enum GroupChild {
    Group(Box<Group>),
    Directive,
    Endif,
    Token(LocationHistory<PreprocessingToken>),
}

#[derive(Debug, Clone)]
pub enum GroupKind {
    If(IfGroupKind),
    Else(),
    Elif,
    /// Global group, in a file with no directives, every token is in this group
    Global,
}

#[derive(Debug, Clone)]
pub enum IfGroupKind {
    Ifdef,
    Ifndef,
    If(LocationHistory<Expression>),
}
#[derive(Debug)]
pub struct PP {
    current_group: Group,
}

impl PP {
    pub fn new() -> Self {
        Self {
            current_group: Group {
                kind: GroupKind::Global,
                children: vec![],
            },
        }
    }
    pub fn parse_translation_unit(&mut self, token_stream: &mut TokenSubset) -> PPR<()> {
        while let Ok(Some(processed)) = self.process_token(token_stream) {}

        Ok(())
    }
    // fn peek(&mut self, token_stream: &mut TokenSubset) -> PPR<&PreprocessingToken> {
    //     match token_stream.peek() {
    //         Some(t) => Ok(t),
    //         None => Ok(),
    //     }
    // }
    pub fn process_token(&mut self, token_stream: &mut TokenSubset) -> PPR<Option<()>> {
        let next = token_stream.peek();
        if next.is_none() {
            return Ok(None);
        };
        let next = next.unwrap();
        match &next.value {
            PreprocessingToken::Punctuator(Punctuator::Hash) if next.location.start.col == 0 => {
                
            }
            _ => {
                self.current_group.children.push(GroupChild::Token((token_stream.next().unwrap()).clone()));
            }
        }
        Ok(Some(()))
    }
    // pub fn collect_until_closer(&mut self, token_stream: &mut TokenSubset) -> (Vec<GroupChild>, Option<GroupChild>) {
    //     let mut c = vec![];
    //     let mut last = None;
    //     while let Ok(Some(group)) = self.parse_group_child(token_stream) {
    //         match group {
    //             GroupChild::Group(ref inner_group) => match inner_group.deref().kind {
    //                 GroupKind::Else() | GroupKind::Elif => {
    //                     // c.push(group.clone());

    //                     // for child in &inner_group.children {
    //                     //     c.push(child.clone());
    //                     // }
    //                     // return c;
    //                     last = Some(group.clone());;
    //                     break;

    //                 }
    //                 _ => {
    //                     c.push(group);
    //                 }
    //             },
    //             GroupChild::Endif => return c,
    //             _ => {
    //                 c.push(group);
    //             }
    //         }
    //     }
    //     (c, last)
    // }
    // pub fn collect_until_newline<'a, 'b: 'a>(&'b self, token_stream: &'b mut TokenSubset) -> Vec<&'a LocationHistory<PreprocessingToken>> {
    //     let mut c: Vec<&LocationHistory<PreprocessingToken>> = vec![];
    //     while let Some(token) = token_stream.peek() {
    //         match token.value {
    //             PreprocessingToken::Newline => {
    //                 break;
    //             }
    //             _ => c.push(token_stream.next().unwrap()),
    //         }
    //     }
    //     c
    // }
    // /// Continually call parse_grop_child until it fails
    // pub fn parse_group_children(&mut self, token_stream: &mut TokenSubset) -> PPR<Vec<GroupChild>> {
    //     let mut children = vec![];
    //     while let Some(_) = token_stream.peek() {
    //         children.push(self.parse_group_child(token_stream)?.unwrap());
    //     }
    //     Ok(children)
    // }
    pub fn pp_token_to_token<'a, T: IntoIterator<Item = &'a LocationHistory<PreprocessingToken>>>(&self, token_stream: T) -> Vec<LocationHistory<Token>> {
        let mut tokens = vec![];

        for token in token_stream {
            tokens.push(token.same(match &token.value {
                PreprocessingToken::Identifier(i) if Keyword::from_str(&i).is_some() => Token::Keyword(Keyword::from_str(&i).unwrap()),
                PreprocessingToken::Identifier(identifier) => Token::Identifier(identifier.to_owned()),
                PreprocessingToken::Number(n) if !n.contains(".") => {
                    Token::Constant(crate::preprocess::ast::Constant::Integer(IntConstant::parse(&token.same(n.to_owned())).unwrap()))
                }
                PreprocessingToken::Punctuator(punctuator) => Token::Punctuator(punctuator.to_owned()),

                PreprocessingToken::CharacterConstant => todo!(),
                PreprocessingToken::StringLiteral(_) => todo!(),
                PreprocessingToken::Error(_) => todo!(),
                PreprocessingToken::Newline | PreprocessingToken::Whitespace(_) => continue,
                x => unreachable!("x{:?} is unreachable", x),
            }));
        }
        tokens
    }
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
    //             }
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
