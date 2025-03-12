use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{lexer::Punctuator, preprocess::ast::Token};

use super::{ast::*, ParseResult, Parser, L};
impl<'a> Parser<'a> {
    pub fn parse_external_declaration(&mut self, expect_semicolon: bool) -> ParseResult<Option<L<ExternalDeclaration>>> {
        if self.peek_token().is_err() {
            return Ok(None);
        }

        let declaration = self.parse_declaration(expect_semicolon)?;
        let next = self.peek_token()?;
        match next {
            L {
                value: Token::Punctuator(Punctuator::Semicolon),
                ..
            } => {
                self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;
                Ok(Some(declaration.shell().same(ExternalDeclaration::Declaration(declaration))))
            }
            L {
                value: Token::Punctuator(Punctuator::LBrace),
                ..
            } => {
                let body = self.parse_compound_statement()?;

                // parse_declaration calls parse_declarator, which when encountering a function pushes a scope
                self.pop_scope();
                Ok(Some(declaration.shell().same(ExternalDeclaration::FunctionDefinition(declaration, body))))
            }

            real => Err(Diagnostic::error().with_message("Expected `;` or `{`").with_labels(vec![
                Label::secondary(real.file_id(), real.location_range()).with_message("`;` forms a declaration, `{` starts the body for a function"),
                Label::primary(declaration.file_id(), declaration.location_range()).with_message("For this declaration"),
            ])),
        }
    }
    pub fn parse_declaration(&mut self, expect_semicolon: bool) -> ParseResult<L<Declaration>> {
        let start = self.peek_token()?;

        let specifiers = self.parse_specifier_list();
        let declarators = self.parse_init_declarators(expect_semicolon)?;

        if let Some(indloc!(DeclarationSpecifier::StorageClass(StorageClass::Typedef))) = specifiers.first() {
            for declarator in &declarators {
                self.define_typedef(&declarator.value.declarator);
            }
        }

        Ok(start.shell().until_last(&declarators).same(Declaration {
            specifiers,
            init_delcarators: declarators,
        }))
        // if self.consume_if_present(&Token::Punctuator(Punctuator::Equal))?.value {
        //     let expr = self.parse_expression(0)?.unwrap();
        //     Ok(start.shell().until_last(&declarators).same(Declaration {
        //         specifiers,
        //         declarators,
        //         init: Some(expr),
        //     }))
        // } else {
        //     Ok(start.shell().until_last(&declarators).same(Declaration {
        //         specifiers,
        //         declarators,
        //         init: None,
        //     }))
        // }
    }
    pub fn parse_init_declarators(&mut self, expect_semicolon: bool) -> ParseResult<Vec<L<InitDeclarator>>> {
        let mut declarators = vec![];

        while let Ok(token) = self.peek_token() {
            if matches!(token.value, Token::Punctuator(Punctuator::Semicolon | Punctuator::Equal | Punctuator::LBrace)) {
                break;
            }
            if declarators.len() > 0 {
                dbg!(&declarators);
                self.expect_token_trace(&Token::Punctuator(Punctuator::Comma), line!())?;
            }

            let declarator = self.parse_declarator(expect_semicolon)?;
            let init_declarator = if self.consume_if_present(&Token::Punctuator(Punctuator::Equal))?.value {
                let expr = self.parse_expression()?.unwrap();
                declarator.shell().until(&expr).same(InitDeclarator { declarator, init: Some(expr) })
            } else {
                declarator.shell().same(InitDeclarator { declarator, init: None })
            };

            declarators.push(init_declarator);
        }

        Ok(declarators)
    }
    pub fn parse_declarator(&mut self, expect_semicolon: bool) -> ParseResult<L<Declarator>> {
        let has_pointer = self.consume_if_present(&Token::Punctuator(Punctuator::Asterisk))?;
        if has_pointer.value {
            let qualifiers = self.parse_specifier_list();
            let mut this = self.parse_declarator(expect_semicolon)?;

            this.value.derived.insert(0, DerivedDeclarator::Pointer(qualifiers));

            Ok(this)
        } else {
            let mut declarator = if self.consume_if_present(&Token::Punctuator(Punctuator::LParen))?.value {
                let d = self.parse_declarator(expect_semicolon)?;
                self.expect_token(&Token::Punctuator(Punctuator::RParen))?;
                d.shell().same(Declarator {
                    kind: DeclaratorKind::Declarator(Box::new(d)),
                    derived: vec![],
                })
            } else {
                if matches!(self.peek_token()?.value, Token::Identifier(_)) {
                    let identifier = self.parse_identifier()?;

                    let d = identifier.shell().same(Declarator {
                        kind: DeclaratorKind::Identifier(identifier),
                        derived: vec![],
                    });
                    self.define_identifier(&d)?;

                    d
                } else {
                    let d = self.location.shell().same(Declarator {
                        kind: DeclaratorKind::Abstract,
                        derived: vec![],
                    });
                    d
                }
            };

            // TODO: Handle square bracket array syntax
            if self.consume_if_present(&Token::Punctuator(Punctuator::LParen))?.value {
                self.push_scope();
                let (params, is_variadic) = self.parse_parameter_list()?;
                self.expect_token(&Token::Punctuator(Punctuator::RParen))?;

                declarator.value.derived.push(DerivedDeclarator::Function(params, is_variadic));
            }
            if self.consume_if_present(&Token::Punctuator(Punctuator::LBracket))?.value {
                let e = self.parse_expression()?.unwrap();
                crate::preprocess::PreProcessor::assert_expression_constant(&e)?;
                self.expect_token_trace(&Token::Punctuator(Punctuator::RBracket), line!())?;

                declarator.value.derived.push(DerivedDeclarator::Array(e));
            }
            Ok(declarator)
        }
    }
    // pub fn parse_abstract_declarator(&mut self) -> ParseResult<L<Declarator>> {
    //     let has_pointer = self.consume_if_present(&Token::Punctuator(Punctuator::Asterisk))?;
    //     if has_pointer.value {
    //         let mut this = self.parse_abstract_declarator()?;

    //         this.value.derived.insert(0, DerivedDeclarator::Pointer(has_pointer.shell()));

    //         Ok(this)
    //     } else {
    //         let mut declarator = if self.consume_if_present(&Token::Punctuator(Punctuator::LParen))?.value {
    //             let d = self.parse_abstract_declarator()?;
    //             self.expect_token(&Token::Punctuator(Punctuator::RParen))?;
    //             d.shell().same(Declarator {
    //                 identifier: DeclaratorKind::Declarator(Box::new(d)),
    //                 derived: vec![],
    //             })
    //         } else {
    //             let identifier = self.parse_identifier()?;

    //             let d = identifier.shell().same(Declarator {
    //                 identifier: DeclaratorKind::Identifier(identifier),
    //                 derived: vec![],
    //             });
    //             self.define_identifier(&d)?;

    //             d
    //         };

    //         // TODO: Handle square bracket array syntax
    //         if self.consume_if_present(&Token::Punctuator(Punctuator::LParen))?.value {
    //             self.push_scope();
    //             let params = self.parse_parameter_list()?;
    //             self.expect_token(&Token::Punctuator(Punctuator::RParen))?;

    //             declarator.value.derived.push(DerivedDeclarator::Function(params));
    //         }
    //         Ok(declarator)
    //     }
    // }
}
