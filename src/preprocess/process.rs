// use peekmore::PeekMore;

// use crate::{
//     lexer::{PreprocessingToken, Punctuator},
//     location::{LocationHistory, Region},
// };

// use super::{PPResult, TokenStream};

// impl PreProcessor {
//     pub fn process_token<'a>(&mut self, ts: &mut TokenStream<'a>) -> PPResult<Option<Vec<LocationHistory<PreprocessingToken>>>> {
//         let next = self.next_non_whitespace_token(ts);
//         if next.is_err() {
//             return Ok(None);
//         }
//         let next = next.unwrap();

//         let mut output = Vec::new();

//         match &next.value {
//             PreprocessingToken::Punctuator(Punctuator::Hash) if next.location.start.col == 0 => {
//                 output.extend(self.process_directive(ts)?);
//             }
//             PreprocessingToken::Punctuator(Punctuator::Hash) => {
//                 dbg!(&ts);

//                 let tokens = ts.cloned().collect::<Vec<_>>();
//                 let tokens = self.process_token_stream(&mut tokens.iter().peekmore());

//                 let stringified = tokens.into_iter().map(|token| self.stringify_token(&token)).collect::<String>();
//                 dbg!(&stringified);
//                 output.push(next.same(PreprocessingToken::StringLiteral(format!(" {}", stringified))));
//                 // output.push(next.same(PreprocessingToken::StringLiteral(stringified)));
//             }

//             // PreprocessingToken::Punctuator(Punctuator::HashHash) => {
//             //     let previous = output.pop().expect("hashash must follow a token");
//             //     self.next_token(ts)?; // eat ##
//             //     let next = self.next_non_whitespace_token(ts)?;
//             //     let glued = self.glue_tokens(&previous, next);

//             //     output.push(glued);

//             // }
//             // PreprocessingToken::Punctuator(Punctuator::Hash)
//             PreprocessingToken::Error(x) => panic!("encountered error token {}", x),
//             PreprocessingToken::Newline => {}

//             PreprocessingToken::Whitespace(w) => {
//                 output.push(next.clone());
//             }
//             _ if self.should_skip() => {
//                 return Ok(Some(vec![]));
//             }
//             PreprocessingToken::Identifier(identifier) if identifier == "defined" => {
//                 // remove parenthesis from `defined(identifier)`, this just makes things easier and is effectively the same
//                 if matches!(self.peek_non_whitespace_token(ts, 0)?.value, PreprocessingToken::Punctuator(Punctuator::LParen)) {
//                     assert_eq!(self.next_non_whitespace_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::LParen));
//                     let macro_name = self.next_token(ts)?;
//                     assert_eq!(self.next_non_whitespace_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::RParen));

//                     output.push(next.shell().same(PreprocessingToken::Identifier("defined".to_string())));
//                     output.push(macro_name.clone());
//                 } else {
//                     let macro_name = self.next_token(ts)?;

//                     output.push(next.shell().same(PreprocessingToken::Identifier("defined".to_string())));
//                     output.push(macro_name.clone());
//                 }
//             }
//             PreprocessingToken::Identifier(identifier) => {
//                 let mac = self.macros.get(identifier).cloned();
//                 match mac {
//                     Some(Macro::Object(mac)) => {
//                         let mut expanded = self.process_token_stream(&mut mac.iter().peekmore());

//                         if expanded.len() > 0 {
//                             let start = expanded.first().unwrap().location.start;
//                             let end = expanded.last().unwrap().location.end;

//                             for token in &mut expanded {
//                                 // TODO: does this make sense? to more or less erase the precise information about where each expanded token was defined?
//                                 // Without doing this the type errors dont seem correct at all
//                                 token.origin.push(Region {
//                                     start,
//                                     end,
//                                     file_id: token.file_id(),
//                                 });

//                                 token.location = next.location.clone();
//                             }
//                             // dbg!(&mac, &expanded);

//                             output.extend(expanded);
//                         }
//                     }
//                     Some(Macro::Function(parameters, mut replacement_list)) => {
//                         // let replacement_list = self.process_token_stream(&mut replacement_list.iter().peekmore());

//                         let mut arguments: Vec<Vec<LocationHistory<PreprocessingToken>>> = vec![];
//                         assert_eq!(self.next_non_whitespace_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::LParen));

//                         let mut parenthesis_balance = 0;
//                         while let Ok(next) = self.peek_token(ts) {
//                             if matches!(next.value, PreprocessingToken::Punctuator(Punctuator::LParen)) {
//                                 parenthesis_balance += 1;
//                             }
//                             if matches!(next.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
//                                 parenthesis_balance -= 1;
//                             }
//                             if parenthesis_balance <= 0 && matches!(next.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
//                                 break;
//                             }

//                             if let PreprocessingToken::Punctuator(Punctuator::Comma) = self.peek_token(ts)?.value {
//                                 assert_eq!(self.next_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::Comma));
//                             }
//                             // if arguments.len() > 0 && arguments.len() > 0 && arguments.last().unwrap().len() > 0{
//                             //     assert_eq!(self.next_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::Comma));
//                             // }

//                             let arg = self.collect_tokens_until_comma(ts)?;
//                             // let expanded_arg = self.process_token_stream(&mut arg.iter().peekmore());
//                             arguments.push(arg);
//                         }

//                         assert_eq!(self.next_token(ts)?.value, PreprocessingToken::Punctuator(Punctuator::RParen));
//                         // dbg!(&arguments);

//                         let mut patched_replacement_list = vec![];

//                         let start = replacement_list.first().unwrap().location.start;
//                         let end = replacement_list.last().unwrap().location.end;

//                         for token in &mut replacement_list {
//                             token.origin.push(Region {
//                                 start,
//                                 end,
//                                 file_id: token.file_id(),
//                             });

//                             token.location = next.location.clone();
//                             match &token.value {
//                                 PreprocessingToken::Identifier(i) => match parameters.iter().position(|arg| arg == i) {
//                                     Some(parameter_index) => {
//                                         // dbg!(&arguments, &parameters);
//                                         patched_replacement_list.extend(arguments[parameter_index].clone());
//                                     }
//                                     None => {
//                                         patched_replacement_list.push(token.same(token.value.clone()));
//                                     }
//                                 },
//                                 _ => patched_replacement_list.push(token.same(token.value.clone())),
//                             }
//                         }
//                         let mut replacement_stream = patched_replacement_list.iter().peekmore();

//                         let expanded = self.process_token_stream(&mut replacement_stream);

//                         output.extend(expanded);
//                     }
//                     _ => {
//                         // panic!("{}", identifier);
//                         output.push(next.same(PreprocessingToken::Identifier(identifier.to_string())));
//                     }
//                 }
//             }
//             x @ _ => {
//                 output.push(next.same(x.clone()));
//             }
//         }
//         // Ok(output)

//         if let Ok(token) = self.peek_non_whitespace_token(ts, 0) {
//             if matches!(token.value, PreprocessingToken::Punctuator(Punctuator::HashHash)) {
//                 // Consume ##
//                 self.next_non_whitespace_token(ts)?;
//                 let next = self.next_non_whitespace_token(ts)?;
//                 let e = output.pop().unwrap();
//                 // dbg!(&next, &e);
//                 let glued = self.glue_tokens(&e, &next);
//                 // dbg!(&glued);
//                 output.push(glued);
//                 dbg!(&output);
//             }
//         }
//         // else if matches!(token.value, PreprocessingToken::Punctuator(Punctuator::Hash)) {
//         //     self.next_non_whitespace_token(ts)?;
//         //     let next = self.next_non_whitespace_token(ts)?;
//         //     let stringified = self.stringify_token(&next);
//         //     output.push(next.same(PreprocessingToken::StringLiteral(stringified)));

//         // }
//         // }

//         Ok(Some(output))
//     }
// }
