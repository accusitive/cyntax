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
type L<T> = LocationHistory<T>;

macro_rules! loc {
    ($p: pat) => {
        L { value: $p, .. }
    };
}
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
    groups: Vec<Group>,
    current_group: Vec<usize>, // current_group: Group,
}

impl PP {
    pub fn new() -> Self {
        Self {
            groups: vec![Group {
                kind: GroupKind::Global,
                children: vec![],
            }],
            current_group: vec![0],
        }
    }
    pub fn parse_translation_unit(&mut self, token_stream: &mut TokenSubset) -> PPR<TranslationUnit> {
        let mut children = vec![];
        while let Ok(Some(processed)) = self.process_token(token_stream) {
            children.push(processed);
        }
        dbg!(&children);

        Ok(TranslationUnit{
            groups: children
        })
    }
    fn next_non_whitespace_token<'a>(&self, token_stream: &'a mut TokenSubset) -> PPR<&'a LocationHistory<PreprocessingToken>> {
        match token_stream.next() {
            Some(loc!(PreprocessingToken::Whitespace(_))) => self.next_non_whitespace_token(token_stream),
            Some(tok) => Ok(tok),
            None => Err(Diagnostic::error().with_message("out of tokens")),
        }
    }
    pub fn process_token(&self, token_stream: &mut TokenSubset) -> PPR<Option<GroupChild>> {
        // let mut tokens = vec![];
        let token = token_stream.peek();
        if token.is_none() {
            return Ok(None);
        };
        let next = token.unwrap();
        match &next.value {
            PreprocessingToken::Punctuator(Punctuator::Hash) if next.location.start.col == 0 => {
                // skip if only a newline
                if matches!(token_stream.peek(), Some(loc!(PreprocessingToken::Newline))) {
                    token_stream.next().unwrap();
                    // skip
                    return self.process_token(token_stream);
                } else {
                    // eat #
                    token_stream.next().unwrap();
                }

                let directive = self.next_non_whitespace_token(token_stream)?.clone();
                let directive = directive.as_identifier().unwrap();
                match directive.as_str() {
                    "ifdef" => {
                        let mut group_body = vec![];
                        while let Ok(Some(t)) = self.process_token(token_stream) {
                            match &t {
                                GroupChild::Endif => break,
                                GroupChild::Group(g) => {
                                    if matches!(g.kind, GroupKind::Else()) {
                                        break;
                                    } else {
                                        group_body.push(t)   
                                    }
                                }
                                _ => group_body.push(t),
                            }
                        }
                        return Ok(Some(GroupChild::Group(Box::new(Group {
                            kind: GroupKind::If(IfGroupKind::Ifdef),
                            children: group_body,
                        }))));
                    }
                    "else" => {
                        let mut group_body = vec![];
                        while let Ok(Some(t)) = self.process_token(token_stream) {
                            match t {
                                GroupChild::Endif => break,
                                _ => group_body.push(t),
                            }
                        }
                        return Ok(Some(GroupChild::Group(Box::new(Group {
                            kind: GroupKind::Else(),
                            children: group_body,
                        }))));
                    }
                    "endif" => {
                        return Ok(Some(GroupChild::Endif));
                        // unreachable!()
                    }
                    _ => {}
                }
                dbg!(&directive);
                Err(Diagnostic::bug())
            }
            _ => {
                dbg!(&self.groups, &self.current_group);
                Ok(Some(GroupChild::Token(token_stream.next().unwrap().clone())))
            }
        }
        // Ok(Some(tokens))
    }

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
    pub fn s(&self, t: &TranslationUnit) -> String {
        let mut s = String::new();
        for group in &t.groups {
            self.s_g(&mut s, group, 0);
        }
        s
    }

    pub fn s_g(&self, s: &mut String, g: &GroupChild, indent: usize) {
        let indent_str = "|".to_string() + &(" ".repeat(indent));
        match g {
            GroupChild::Group(group) => {
                s.push_str(&indent_str);
                match &group.kind {
                    GroupKind::If(IfGroupKind::If(_)) => s.push_str("#if"),
                    GroupKind::If(IfGroupKind::Ifdef) => s.push_str("#ifdef"),
                    GroupKind::If(IfGroupKind::Ifndef) => s.push_str("#ifndef"),
                    GroupKind::Else() => s.push_str("#else"),
                    GroupKind::Elif => s.push_str("#elif"),
                    GroupKind::Global => {},
                                    }
                s.push('\n');
                for child in &group.children {
                    self.s_g(s, child, indent + 2);
                }
            }
            GroupChild::Directive => {
                s.push_str("#");
            }
            GroupChild::Endif => {
                s.push_str(&indent_str);
                s.push_str("#endif\n");
            }
            GroupChild::Token(location_history) => {
                s.push_str(&indent_str);
                s.push_str(&self.stringify_token(location_history));
                s.push('\n');
            }
        }
    }

    pub fn stringify_token(&self, token: &LocationHistory<PreprocessingToken>) -> String {
        match &token.value {
            PreprocessingToken::Identifier(i) => i.to_string(),
            PreprocessingToken::Number(num) => num.to_string(),
            PreprocessingToken::StringLiteral(s) => s.to_string(),
            PreprocessingToken::Punctuator(punctuator) => punctuator.stringify().to_string(),
            PreprocessingToken::Whitespace(w) => w.to_string(),
            PreprocessingToken::Newline => "".to_string(),
            token => unimplemented!("token {:?} is not stringifiable yet", &token),
        }
    }
}
