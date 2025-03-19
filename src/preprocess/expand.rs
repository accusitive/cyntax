use std::{net::ToSocketAddrs, ops::Deref};

use codespan_reporting::diagnostic::Diagnostic;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    lexer::{Lexer, PreprocessingToken, Punctuator},
    location::{LocationHistory, Region},
    parser::{expression::ParseExpressionOptions, Parser},
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
                    v.extend(self.expand_group_children(&group.content)?);
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref())?);
                    }
                }
            }
            super::tree::GroupKind::IfNDef(macro_name, elze) => {
                if self.macros.get(macro_name).is_none() {
                    v.extend(self.expand_group_children(&group.content)?);
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
                    files: self.files.clone(),
                    symbol_stack: vec![],
                    location: LocationHistory::x(()),
                };
                let condition = parser
                    .parse_expression_2(ParseExpressionOptions {
                        stage: crate::parser::expression::ParseStage::Preprocess,
                        comma_viable: false,
                        min_prec: 0,
                    })?
                    .unwrap();
                self.files = parser.files;
                if self.evaluate_constant_expression(&condition).unwrap() != 0 {
                    v.extend(self.expand_group_children(&group.content)?);
                } else {
                    if let Some(group) = elze {
                        v.extend(self.expand_group(group.deref())?);
                    }
                }
            }
            super::tree::GroupKind::Else => {
                v.extend(self.expand_group_children(&group.content)?);
            }
            super::tree::GroupKind::Body => {
                v.extend(self.expand_group_children(&group.content)?);
            }
        }
        Ok(v)
    }
    pub fn expand_group_children(&mut self, group_children: &[GroupChild]) -> PPResult<Vec<LocationHistory<PreprocessingToken>>> {
        let mut v = vec![];
        let mut peekable = group_children.iter().peekmore();

        while let Some(child) = peekable.next() {
            dbg!(&child, &peekable);
            
            match child {
                GroupChild::Token(defined_l @ loc!(PreprocessingToken::Identifier(identifier))) if identifier == "defined" => {
                    let next = peekable.peek();
                    if let Some(GroupChild::Token(macro_name_l @ loc!(PreprocessingToken::Identifier(macro_name)))) = next {
                        peekable.next().unwrap();
                        let one = macro_name_l.same(PreprocessingToken::Number("1".to_string()));
                        v.push(one);
                    } else {
                        peekable.next().unwrap();
                        let zero = defined_l.same(PreprocessingToken::Number("0".to_string()));
                        v.push(zero);
                    }
                }
                GroupChild::Token(macro_name_l @ loc!(PreprocessingToken::Identifier(m))) if self.is_function_style_macro(m) => {
                    let mac = self.macros.get(m).unwrap().as_function().unwrap();
                    // let e = self.expand_function_macros(&mut peekable, macro_name_l, mac.0, mac.1);
                    panic!();
                }
                _ => {
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

                // loc!(PreprocessingToken::Identifier(identifier)) if identifier == "defined" => {
                //     let macro_name = self.next_non_whitespace_token(token_stream)?;

                //     if let Some(_) = self.macros.get(macro_name.value.as_identifier().unwrap()) {
                //         let one = token.same(PreprocessingToken::Number("1".to_string()));
                //         Ok(vec![one])
                //     } else {
                //         let zero = token.same(PreprocessingToken::Number("0".to_string()));
                //         Ok(vec![zero])
                //     }
                // }
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

                super::tree::DirectiveKind::DefineFunction(macro_name, parameters, replacement_list) => {
                    let macro_name_identifier = macro_name.as_identifier().unwrap();
                    self.macros.insert(
                        macro_name_identifier.to_string(),
                        crate::preprocess::Macro::Function(parameters.clone(), replacement_list.to_vec()),
                    );
                    Ok(vec![])
                }
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
                    let file_id = self.files.add(header_name.as_header_name().unwrap().0.to_string(), path.clone());
                    let mut lexer = Lexer::from(path.as_str());
                    let lexer_tokens = lexer.tokenize().into_iter().map(|tok| tok.double(file_id)).collect::<Vec<_>>();
                    let stretches = self.create_token_stretches(&mut lexer_tokens.iter().peekmore())?;
                    let all_groups = self.parse_all_groups(&mut stretches.iter().peekmore());

                    let mut expanded_tokens = vec![];
                    for group in &all_groups {
                        expanded_tokens.extend(self.expand_group(group)?);
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
                        // Macro::Function(parameters, replacement_list) => {
                        //     dbg!(&tokens);
                        //     panic!();
                        //     output.extend(self.expand_function_macros(tokens, &token, parameters.clone(), replacement_list))
                        // },
                        _ => {}
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
    pub fn expand_function_macros(
        &self,
        // tokens: &[L<PreprocessingToken>],
        stream: &mut PeekMoreIterator<std::slice::Iter<'_, LocationHistory<PreprocessingToken>>>,
        token: &L<PreprocessingToken>,
        parameters: Vec<String>,
        replacement_list: &Vec<L<PreprocessingToken>>,
    ) -> Vec<L<PreprocessingToken>> {
        // let mut stream: peekmore::PeekMoreIterator<std::slice::Iter<'_, LocationHistory<PreprocessingToken>>> = tokens.iter().peekmore();
        let macro_name = self.next_non_whitespace_token(stream).unwrap();
        // dbg!(&stream);
        assert!(matches!(
            self.next_non_whitespace_token(stream),
            Ok(loc!(PreprocessingToken::Punctuator(Punctuator::LParen)))
        ));
        let mut args: Vec<Vec<L<PreprocessingToken>>> = vec![];
        while let Some(token) = stream.peek() {
            let mut argument = vec![];
            if matches!(token, loc!(PreprocessingToken::Punctuator(Punctuator::RParen))) {
                // push arg
                break;
            }

            while let Some(token) = stream.peek() {
                if matches!(token, loc!(PreprocessingToken::Punctuator(Punctuator::Comma))) {
                    // push arg
                    break;
                }
                argument.push(token.to_owned().clone());
            }
            args.push(argument);
        }
        dbg!(&args);

        assert!(matches!(
            self.next_non_whitespace_token(stream),
            Ok(loc!(PreprocessingToken::Punctuator(Punctuator::RParen)))
        ));
        panic!();
        // let replacement_list = replacement_list
        //     .iter()
        //     .cloned()
        //     .map(|mut replacement_token| {
        //         replacement_token.origin.push(Region {
        //             start,
        //             end,
        //             file_id: token.file_id(),
        //         });
        //         replacement_token.location = token.location.clone();

        //         replacement_token
        //     })
        //     .collect::<Vec<_>>();

        // let expanded = self.expand_tokens(&replacement_list);

        vec![]
    }
    pub fn is_function_style_macro(&self, macro_name: &str) -> bool {
        match self.macros.get(macro_name) {
            Some(Macro::Function(_, _)) => true,
            _ => false,
        }
    }
}
