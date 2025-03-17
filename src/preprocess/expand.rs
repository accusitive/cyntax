use std::ops::Deref;

use codespan_reporting::diagnostic::Diagnostic;
use peekmore::PeekMore;

use crate::{
    lexer::{Lexer, PreprocessingToken},
    location::{LocationHistory, Region},
    parser::Parser,
    preprocess::tree::DirectiveKind,
};

use super::{
    tree::{Group, GroupChild},
    Macro, PPResult, Preprocessor, L,
};

impl Preprocessor {
    pub fn expand_group(&mut self, group: &Group) -> PPResult<Vec<L<PreprocessingToken>>> {
        let mut v = vec![];

        match &group.kind {
            super::tree::GroupKind::IfDef(macro_name, elze) => {
                if self.macros.get(macro_name).is_some() {
                    for child in &group.content {
                        v.extend(self.expand_group_child(child)?);
                    }
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref())?);
                    }
                }
            }
            super::tree::GroupKind::IfNDef(macro_name, elze) => {
                if self.macros.get(macro_name).is_none() {
                    for child in &group.content {
                        v.extend(self.expand_group_child(child)?);
                    }
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref())?);
                    }
                }
            }
            super::tree::GroupKind::If(condition, elze) | super::tree::GroupKind::Elif(condition, elze) => {
                let tokens: Vec<_> = self
                    .expand_tokens(&condition)
                    .iter()
                    .filter_map(|token| Preprocessor::pp_token_to_token(token.clone()))
                    .collect();

                let mut parser = Parser {
                    tokens: tokens.iter().peekmore(),
                    files: codespan_reporting::files::SimpleFiles::new(),
                    symbol_stack: vec![],
                    location: LocationHistory::x(()),
                };
                let condition = parser.parse_expression().unwrap().unwrap();
                if self.evaluate_constant_expression(&condition).unwrap() != 0 {
                    for child in &group.content {
                        v.extend(self.expand_group_child(child)?);
                    }
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref())?);
                    }
                }
            }
            super::tree::GroupKind::Else => {
                for child in &group.content {
                    v.extend(self.expand_group_child(child)?);
                }
            }
            super::tree::GroupKind::Body => {
                for child in &group.content {
                    v.extend(self.expand_group_child(child)?);
                }
            }
        }
        Ok(v)
    }
    pub fn expand_group_child(&mut self, group_child: &GroupChild) -> PPResult<Vec<L<PreprocessingToken>>> {
        match group_child {
            GroupChild::Token(token) => match token {
                // loc!(PreprocessingToken::Identifier(identifier)) if self.macros.get(identifier).is_some() => match self.macros.get(identifier) {
                //     Some(Macro::Object(replacement_list)) => Ok(self.expand_object_macro(token, replacement_list)),
                //     _ => Ok(vec![token.clone()]),
                // },
                loc!(PreprocessingToken::Identifier(identifier)) if self.macros.get(identifier).is_some() => Ok(self.expand_tokens(&[token.clone()])),
                _ => Ok(vec![token.clone()]),
            },
            // define and the like
            GroupChild::Directive(directive_kind) => match directive_kind {
                super::tree::DirectiveKind::DefineObject(macro_name, replacement_list) => {
                    let macro_name_identifier = macro_name.as_identifier().unwrap();
                    self.macros
                        .insert(macro_name_identifier.to_string(), crate::preprocess::Macro::Object(replacement_list.to_vec()));
                    Ok(vec![])
                }
                super::tree::DirectiveKind::DefineFunction(_) => todo!(),
                DirectiveKind::Undefine(macro_name) => {
                    self.macros.remove(macro_name.as_identifier().unwrap());
                    Ok(vec![])
                }
                DirectiveKind::Error(reason) => Err(Diagnostic::error()
                    .with_message("Encountered error directive")
                    .with_message(self.stringify_token(0, reason))
                    .with_labels(reason.generate_location_labels())),
                DirectiveKind::Include(header_name) => {
                    let path = self.locate_header(header_name).unwrap();
                    let mut lexer = Lexer::from(path.as_str());
                    let lexer_tokens = lexer.tokenize().into_iter().map(|tok| tok.double(0)).collect::<Vec<_>>();
                    let stretches = self.create_token_stretches(&mut lexer_tokens.iter().peekmore())?;
                    let all_groups = self.parse_all_groups(&mut stretches.iter().peekmore());

                    let mut expanded_tokens = vec![];
                    for group in &all_groups {
                        expanded_tokens.extend(self.expand_group(group).unwrap());
                    }

                    // let expanded = self.expand_tokens(&expanded_tokens);
                    Ok(expanded_tokens)
                }
                _ => unreachable!(),
            },
            GroupChild::Group(group) => self.expand_group(group),
        }
    }
    fn expand_tokens(&self, tokens: &[L<PreprocessingToken>]) -> Vec<L<PreprocessingToken>> {
        let mut output = vec![];
        for token in tokens {
            match &token.value {
                PreprocessingToken::Identifier(identifier) if self.macros.get(identifier).is_some() => {
                    let r#macro = self.macros.get(identifier).unwrap();
                    match r#macro {
                        Macro::Object(replacement_list) => output.extend(self.expand_object_macro(&token, replacement_list)),
                    }
                }
                _ => {
                    output.push(token.clone());
                }
            }
        }

        output
    }
    fn expand_object_macro(&self, token: &L<PreprocessingToken>, replacement_list: &Vec<L<PreprocessingToken>>) -> Vec<L<PreprocessingToken>> {
        if replacement_list.len() > 0 {
            let start = replacement_list.first().unwrap().location.start;
            let end = replacement_list.last().unwrap().location.end;
            //update the origin field on each token in replacement_list
            let replacement_list = replacement_list
                .iter()
                .cloned()
                .map(|mut replacement_token| {
                    replacement_token.origin.push(Region {
                        start,
                        end,
                        file_id: token.file_id(),
                    });
                    replacement_token.location = token.location.clone();

                    replacement_token
                })
                .collect::<Vec<_>>();

            self.expand_tokens(&replacement_list)
        } else {
            vec![]
        }
    }
}
