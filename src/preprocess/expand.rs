use std::ops::Deref;

use peekmore::PeekMore;

use crate::{lexer::PreprocessingToken, location::LocationHistory, parser::Parser};

use super::{
    tree::{Group, GroupChild},
    Preprocessor, L,
};

impl Preprocessor {
    pub fn expand_group(&mut self, group: &Group) -> Vec<L<PreprocessingToken>> {
        let mut v = vec![];

        match &group.kind {
            super::tree::GroupKind::IfDef(macro_name, elze) => {
                if self.macros.get(macro_name).is_some() {
                    for child in &group.content {
                        v.extend(self.expand_group_child(child));
                    }
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref()));
                    }
                }
            }
            super::tree::GroupKind::IfNDef(macro_name, group) => todo!(),
            super::tree::GroupKind::If(items, group) => todo!(),
            super::tree::GroupKind::Elif(condition, elze) => {
                let tokens: Vec<_> = condition.iter().filter_map(|token| Preprocessor::pp_token_to_token(token.clone())).collect();

                let mut parser = Parser {
                    tokens: tokens.iter().peekmore(),
                    files: codespan_reporting::files::SimpleFiles::new(),
                    symbol_stack: vec![],
                    location: LocationHistory::x(()),
                };
                let condition = parser.parse_expression().unwrap().unwrap();
                if self.evaluate_constant_expression(&condition).unwrap() != 0 {
                    for child in &group.content {
                        v.extend(self.expand_group_child(child));
                    }
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref()));
                    }
                }
            }
            super::tree::GroupKind::Else => {
                for child in &group.content {
                    v.extend(self.expand_group_child(child));
                }
            }
            super::tree::GroupKind::Body => {
                for child in &group.content {
                    v.extend(self.expand_group_child(child));
                }
            }
        }
        v
    }
    pub fn expand_group_child(&mut self, group_child: &GroupChild) -> Vec<L<PreprocessingToken>> {
        match group_child {
            GroupChild::Token(location_history) => vec![location_history.clone()],
            // define and the like
            GroupChild::Directive(directive_kind) => match directive_kind {
                super::tree::DirectiveKind::DefineObject(macro_name, replacement_list) => {
                    let macro_name_identifier = macro_name.as_identifier().unwrap();
                    self.macros
                        .insert(macro_name_identifier.to_string(), crate::preprocess::Macro::Object(replacement_list.to_vec()));
                    vec![]
                }
                super::tree::DirectiveKind::DefineFunction(location_history) => todo!(),
                _ => unreachable!(),
            },
            GroupChild::Group(group) => self.expand_group(group),
        }
    }
}
