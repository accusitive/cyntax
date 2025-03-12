use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::preprocess::ast::Token;

use super::{ast::*, ParseResult, Parser, L};
impl<'a> Parser<'a> {
    pub fn parse_specifier_list(&mut self) -> Vec<L<DeclarationSpecifier>> {
        let mut v = vec![];
        while let Ok(Some(specifier)) = self.maybe_parse_specifier() {
            v.push(specifier);
        }
        v
    }
    #[rustfmt::skip]
    fn maybe_parse_specifier(&mut self) -> ParseResult<Option<L<DeclarationSpecifier>>> {
        let next = self.peek_token()?;
        match &next.value {
            Token::Keyword(Keyword::Typedef) |
            Token::Keyword(Keyword::Extern) |
            Token::Keyword(Keyword::Static) |
            Token::Keyword(Keyword::Auto) |
            Token::Keyword(Keyword::Register) |
            Token::Keyword(Keyword::Const) |
            Token::Keyword(Keyword::Restrict) |
            Token::Keyword(Keyword::Volatile) |
            Token::Keyword(Keyword::Void) |
            Token::Keyword(Keyword::Char) |
            Token::Keyword(Keyword::Short) |
            Token::Keyword(Keyword::Int) |
            Token::Keyword(Keyword::Long) |
            Token::Keyword(Keyword::Float) |
            Token::Keyword(Keyword::Double) |
            Token::Keyword(Keyword::Signed) |
            Token::Keyword(Keyword::Unsigned) |
            Token::Keyword(Keyword::Struct) |
            Token::Keyword(Keyword::Union)

            => Ok(Some(self.parse_specifier()?)),

            Token::Identifier(identifier) if self.resolve_typedef2(identifier) => Ok(Some(self.parse_specifier()?)),

            _ => Ok(None)
        }
    }

    fn parse_specifier(&mut self) -> ParseResult<L<DeclarationSpecifier>> {
        let token = self.next_token()?;
        match &token.value {
            Token::Keyword(Keyword::Typedef) => Ok(token.same(DeclarationSpecifier::StorageClass(StorageClass::Typedef))),
            Token::Keyword(Keyword::Extern) => Ok(token.same(DeclarationSpecifier::StorageClass(StorageClass::Extern))),
            Token::Keyword(Keyword::Static) => Ok(token.same(DeclarationSpecifier::StorageClass(StorageClass::Static))),
            Token::Keyword(Keyword::Auto) => Ok(token.same(DeclarationSpecifier::StorageClass(StorageClass::Auto))),
            Token::Keyword(Keyword::Register) => Ok(token.same(DeclarationSpecifier::StorageClass(StorageClass::Register))),
            Token::Keyword(Keyword::Const) => Ok(token.same(DeclarationSpecifier::TypeQualifier(TypeQualifier::Const))),
            Token::Keyword(Keyword::Restrict) => Ok(token.same(DeclarationSpecifier::TypeQualifier(TypeQualifier::Restrict))),
            Token::Keyword(Keyword::Volatile) => Ok(token.same(DeclarationSpecifier::TypeQualifier(TypeQualifier::Volatile))),
            Token::Keyword(Keyword::Void) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Void))),
            Token::Keyword(Keyword::Char) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Char))),
            Token::Keyword(Keyword::Short) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Short))),
            Token::Keyword(Keyword::Int) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int))),
            Token::Keyword(Keyword::Long) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Long))),
            Token::Keyword(Keyword::Float) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Float))),
            Token::Keyword(Keyword::Double) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Double))),
            Token::Keyword(Keyword::Signed) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Signed))),
            Token::Keyword(Keyword::Unsigned) => Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Unsigned))),

            Token::Keyword(Keyword::Struct) | Token::Keyword(Keyword::Union) => {
                let next = self.peek_token()?;
                let struct_identifier = if let Token::Identifier(struct_identifier) = &next.value {
                    self.define_typename(&next.same(struct_identifier.to_string()));
                    Some(self.parse_identifier()?)
                } else {
                    None
                };

                let spec = if self.consume_if_present(&Token::Punctuator(crate::lexer::Punctuator::LBrace))?.value {
                    self.push_scope();
                    let declarations = self.parse_struct_declaration_list()?;
                    let spec = TypeSpecifier::Struct(StructSpecifier {
                        name: struct_identifier,
                        declarations: StructCompleteness::Complete(declarations),
                    });
                    self.pop_scope();
                    spec
                } else {
                    TypeSpecifier::Struct(StructSpecifier {
                        name: struct_identifier,
                        declarations: StructCompleteness::Incomplete,
                    })
                };
                Ok(token.same(DeclarationSpecifier::TypeSpecifier(spec)))
            }
            Token::Identifier(identifier) if self.resolve_typedef2(identifier) => {
                Ok(token.same(DeclarationSpecifier::TypeSpecifier(TypeSpecifier::TypedefName(identifier.to_string()))))
            }

            // Token::Identifier(identifier) if identifier == "sometype" => Ok(token.same(DeclarationSpecifier::TypeSpecifier(Type::TypedefName(identifier.to_string())))),
            _ => Err(Diagnostic::error().with_message("Not a valid type specifier").with_labels(vec![Label::primary(
                token.file_id(),
                token.location.start.offset..token.location.end.offset,
            )
            .with_message(format!("Found `{}`, expected a valid type specifier", token.value.describe()))])),
        }
    }
    fn parse_struct_declaration_list(&mut self) -> ParseResult<Vec<StructDeclaration>> {
        let mut declarations = vec![];

        while !self.consume_if_present(&Token::Punctuator(crate::lexer::Punctuator::RBrace))?.value {
            let specifiers = self.parse_specifier_list();
            let declarator = self.parse_declarator(true)?;
            let bitfield = if self.consume_if_present(&Token::Punctuator(crate::lexer::Punctuator::Colon))?.value {
                Some(self.parse_expression()?.unwrap())
            } else {
                None
            };

            self.expect_token(&Token::Punctuator(crate::lexer::Punctuator::Semicolon))?;

            declarations.push(StructDeclaration {
                specifiers: specifiers,
                declarator,
                bitfield,
            })
        }

        Ok(declarations)
    }
}
