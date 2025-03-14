use std::collections::HashMap;

use crate::lexer::Punctuator;
use crate::location::LocationHistory;

use super::{pp_token_to_token, Condition, Macro, PPResult, TokenStream};
use crate::lexer::PreprocessingToken;
use crate::parser::Parser;

use crate::parser::expression::ParseExpressionOptions;

use crate::parser::expression::ParseStage;

use crate::lexer::Lexer;

use codespan_reporting::diagnostic::Diagnostic;
use peekmore::PeekMore;

use super::PreProcessor;

impl PreProcessor {
    pub fn process_directive(&mut self, ts: &mut TokenStream) -> PPResult<Vec<LocationHistory<PreprocessingToken>>> {
        let items = if self.consume_if_present(ts, &PreprocessingToken::Newline)?.value {
            Ok(vec![])
        } else {
            if !matches!(self.peek_non_whitespace_token(ts, 0)?.value, PreprocessingToken::Identifier(_)) {
                dbg!(&"returning early on {}", &self.peek_non_whitespace_token(ts, 0)?.value);
                return Ok(vec![])
            }
            let directive = self.next_non_whitespace_token(ts)?;

            match directive.value.as_identifier().unwrap().as_str() {
                "endif" => {
                    // ALways pop, since everything else always pushes *something*
                    self.conditions.pop().unwrap();
                }
                "else" => {
                    if !self.is_active() {
                        self.conditions.pop().unwrap();
                        self.conditions.push(Condition {
                            value: true
                        });
                    } else {
                        self.conditions.pop().unwrap();
                        self.conditions.push(Condition {
                            // kind: ConditionKind::Simple(false),
                            value: false
                        });
                    }
                }
                "elif" => {
                    let condition = self.handle_if(ts)?;

                    if !self.is_active() {
                        self.conditions.pop().unwrap();
                        self.conditions.push(Condition {
                            // kind: ConditionKind::Simple(condition),
                            value: condition
                        });
                    } else {
                        self.conditions.pop().unwrap();
                        self.conditions.push(Condition {
                            // kind: ConditionKind::Simple(true),
                            value: true
                        });
                    }
                }
                "if" => {
                    let condition = self.handle_if(ts)?;
                    if self.is_active() {
                        // If the current block is active, push the condition
                        self.conditions.push(Condition {
                            value: condition
                        });
                    } else {
                        // Otherwise just push `true` to keep the condition stack coherent
                        self.conditions.push(Condition {
                            value: true
                        });
                    }
                }
                "ifdef" => {
                    let i = self.next_non_whitespace_token(ts)?;
                    let exists = self.macros.get(i.value.as_identifier().unwrap()).is_some();
                    self.conditions.push(Condition {
                        value: exists
                    });
                }
                "ifndef" => {
                    let i = self.next_non_whitespace_token(ts)?;
                    let exists = self.macros.get(i.value.as_identifier().unwrap()).is_some();
                    self.conditions.push(Condition {
                        value: !exists
                    });
                }
                _ if self.should_skip() => {
                    let mut skipped_tokens = vec![];
                    // Need to consume until newline
                    while let Ok(token) = self.peek_token(ts) {
                        if matches!(token.value, PreprocessingToken::Newline) {
                            skipped_tokens.push(self.next_token(ts)?);
                            break;
                        }
                        skipped_tokens.push(self.next_token(ts)?);
                    }
                    // dbg!("skipping", &d, &skipped_tokens);

                    return Ok(vec![]);
                }
                "define" => {
                    let identifier = self.next_non_whitespace_token(ts)?.value.as_identifier().unwrap();
                    if let Ok(LocationHistory {
                        value: PreprocessingToken::Punctuator(Punctuator::LParen),
                        ..
                    }) = self.peek_token(ts)
                    {
                        let _lparen = self.next_token(ts)?;
                        // incremented when we encounter (, decremented when we encounter )
                        let mut parameters = vec![];
                        while let Ok(next) = self.peek_token(ts) {
                            if matches!(next.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
                                break;
                            }

                            if parameters.len() > 0 {
                                assert_eq!(self.next_non_whitespace_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::Comma));
                            }
                            let identifier = &self.next_non_whitespace_token(ts)?.value;
                            parameters.push(identifier.as_identifier().unwrap().to_string());
                        }
                        assert_eq!(self.next_non_whitespace_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::RParen));

                        // todo: dedup replacement_list parsing code
                        let mut replacement_list = vec![];
                        while let Ok(next) = self.next_non_whitespace_token(ts) {
                            if matches!(next.value, PreprocessingToken::Newline) {
                                break;
                            }
                            replacement_list.push(next.clone());
                        }

                        self.macros.insert(identifier.to_string(), Macro::Function(parameters, replacement_list));
                        println!("Defined function macro {}", identifier);
                    } else {
                        let mut replacement_list = vec![];
                        while let Ok(next) = self.next_non_whitespace_token(ts) {
                            if matches!(next.value, PreprocessingToken::Newline) {
                                break;
                            }
                            replacement_list.push(next.clone());
                        }
                        self.macros.insert(identifier.to_string(), Macro::Object(replacement_list));
                        println!("Defined object macro {}", identifier);
                    }
                }
                "undef" => {
                    let macro_name = self.next_non_whitespace_token(ts)?.value.as_identifier().unwrap();

                    self.macros.remove(macro_name);
                }
                "include" => {
                    let header_name = self.next_non_whitespace_token(ts)?;

                    dbg!(&header_name);
                    let hn = header_name.value.as_header_name().unwrap();
                    let path = match hn.1 {
                        crate::lexer::HeaderNameKind::H => {
                            format!("/usr/include/{}", hn.0)
                        }
                        crate::lexer::HeaderNameKind::Q => {
                            format!("{}", hn.0)
                        }
                    };
                    dbg!(&path);
                    let header_content = self.include(&hn).unwrap();

                    let mut lexer = Lexer::from(header_content.as_str());

                    let file = self.files.add(path, header_content.clone());
                    let this_file = self.file_id.clone();
                    self.file_id = file;
                    let tokens = lexer.tokenize();
                    let tokens = tokens.iter().map(|t: &crate::location::Located<PreprocessingToken>| t.clone().double(file)).collect::<Vec<_>>();

                    let mut stream = tokens.iter().peekmore();
                    let expanded = self.process_token_stream(&mut stream);

                    self.file_id = this_file;
                    return Ok(expanded);
                }

                "error" => {
                    let reason = self.next_non_whitespace_token(ts)?;
                    return Err(Diagnostic::error()
                        .with_message("encountered #error directive")
                        .with_labels(reason.generate_location_labels()));
                }
                "warning" => {
                    let reason = self.next_non_whitespace_token(ts)?;
                    println!("warning {:#?}", reason.value);
                    // return Err(Diagnostic::warning()
                    //     .with_message("encountered #warning directive")
                    //     .with_labels(reason.generate_location_labels()));
                }
                directive => unimplemented!("skipping #{directive}"),
            }
            Ok(vec![])
        };
        items
    }

    fn handle_if(&mut self, ts: &mut peekmore::PeekMoreIterator<std::slice::Iter<'_, LocationHistory<PreprocessingToken>>>) -> PPResult<bool> {
        let ignore_conditions = std::mem::replace(&mut self.ignore_conditions, true);
        let mut expression_tokens = vec![];
        while let Ok(next) = self.next_non_whitespace_token(ts) {
            if matches!(next.value, PreprocessingToken::Newline) {
                break;
            }
            expression_tokens.push(next.clone());
        }
        // dbg!(&expression_tokens);
        let expression_token_stream = &mut expression_tokens.iter().peekmore();
        // dbg!(&expression_token_stream);
        let expanded = self.process_token_stream(expression_token_stream);
        // dbg!(&expanded);

        let tokens = expanded.iter().map(|token| pp_token_to_token(token.clone())).collect::<Vec<_>>();
        let token_stream = tokens.iter().peekmore();
        let mut p = Parser {
            tokens: token_stream,
            files: self.files.clone(),
            symbol_stack: vec![HashMap::new()],
            location: LocationHistory::x(()),
        };
        let e = p
            .parse_expression_2(ParseExpressionOptions {
                min_prec: 0,
                comma_viable: false,
                stage: ParseStage::Preprocess,
            })?
            .unwrap();

        let evaluated = self.evaluate_constant_expression(&e)?;
        self.ignore_conditions = ignore_conditions;
        Ok(evaluated != 0)
    }
}
