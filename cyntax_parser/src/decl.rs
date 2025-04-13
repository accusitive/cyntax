use crate::ast::*;
use crate::{PResult, Parser};
use cyntax_common::ast::*;
use cyntax_common::spanned::{Span, Spanned};
use cyntax_errors::Diagnostic;
use cyntax_errors::errors::SimpleError;
use cyntax_lexer::span;
/// Declarations, Declarators, Init declarators, stuff of that sort
impl Parser {
    pub fn parse_external_declaration(&mut self) -> PResult<Option<ExternalDeclaration>> {
        if self.token_stream.peek().is_none() {
            return Ok(None);
        }
        let specifiers = self.parse_declaration_specifiers()?;

        let mut init_declarators = self.parse_init_declarators()?;
        dbg!(&self.peek_token());

        // int a;
        if self.eat_if_next(Token::Punctuator(Punctuator::Semicolon))? {
            return Ok(Some(ExternalDeclaration::Declaration(Declaration {
                specifiers: specifiers,
                init_declarators: init_declarators,
            })));
        }

        // int main() {}
        if init_declarators.len() == 1 /* && check to make sure no initializer */ && self.peek_matches(Token::Punctuator(Punctuator::LeftBrace))? {
            let declarator = init_declarators.remove(0).declarator;
            let body = self.parse_statement()?;
            return Ok(Some(ExternalDeclaration::FunctionDefinition(FunctionDefinition { specifiers, declarator, body })));
        }
        // int a,b() {}
        if init_declarators.len() > 1 && self.peek_matches(Token::Punctuator(Punctuator::LeftBrace))? {
            let range = specifiers.span_fallback(self.last_location.clone()).start..self.last_location.end;

            return Err(SimpleError(range, "Declarations with more than 1 declarator cannot have a function body".to_string()).into_why_report());
        }

        dbg!(&self.peek_token());
        unreachable!()
        // if self.eat_if_next(Token::Punctuator(Punctuator::Semicolon))? {
        //     return Ok(Some(ExternalDeclaration::Declaration(Declaration { specifiers, init_declarators: vec![] })));
        // } else if self.can_start_compound_statement() {
        //     let body = self.parse_statement()?;
        //     dbg!(&body);
        //     Ok(Some(ExternalDeclaration::FunctionDefinition(FunctionDefinition { specifiers: specifiers })))
        // } else {
        //     let init_declarators = self.parse_init_declarators()?;
        //     return Ok(Some(ExternalDeclaration::Declaration(Declaration { specifiers, init_declarators })));
        // }
    }
    pub fn parse_declaration(&mut self) -> PResult<Declaration> {
        let specifiers = self.parse_declaration_specifiers()?;
        let init_declarators = self.parse_init_declarators()?;
        Ok(Declaration { specifiers, init_declarators })
    }
    pub fn parse_declaration_specifiers(&mut self) -> PResult<Vec<Spanned<DeclarationSpecifier>>> {
        let mut declaration_specifiers = vec![];
        while self.can_parse_declaration_specifier() {
            let specifier = self.parse_declaration_specifier()?;
            declaration_specifiers.push(specifier);
        }
        Ok(declaration_specifiers.into())
    }
    pub fn can_parse_declaration_specifier(&mut self) -> bool {
        // todo: add typename/identifiers to this
        return matches!(self.token_stream.peek(), Some(span!(Token::Keyword(storage_class!() | type_specifier!() | type_qualifier!() | Keyword::Inline))));
    }
    pub fn parse_declaration_specifier(&mut self) -> PResult<Spanned<DeclarationSpecifier>> {
        let Spanned { value, range } = self.next_token()?;
        Ok(Spanned::new(
            range,
            match value {
                // Storage-class specifiers
                Token::Keyword(Keyword::Typedef) => DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef),
                Token::Keyword(Keyword::Extern) => DeclarationSpecifier::StorageClass(StorageClassSpecifier::Extern),
                Token::Keyword(Keyword::Static) => DeclarationSpecifier::StorageClass(StorageClassSpecifier::Static),
                Token::Keyword(Keyword::Auto) => DeclarationSpecifier::StorageClass(StorageClassSpecifier::Auto),
                Token::Keyword(Keyword::Register) => DeclarationSpecifier::StorageClass(StorageClassSpecifier::Register),

                // Type specifiers
                Token::Keyword(Keyword::Void) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Void),
                Token::Keyword(Keyword::Char) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Char),
                Token::Keyword(Keyword::Short) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Short),
                Token::Keyword(Keyword::Int) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int),
                Token::Keyword(Keyword::Long) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Long),
                Token::Keyword(Keyword::Float) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Float),
                Token::Keyword(Keyword::Double) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Double),
                Token::Keyword(Keyword::Signed) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Signed),
                Token::Keyword(Keyword::Unsigned) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Unsigned),
                Token::Keyword(Keyword::Bool) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Bool),
                Token::Keyword(Keyword::Complex) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Complex),
                Token::Keyword(Keyword::Struct) => DeclarationSpecifier::TypeSpecifier(self.parse_struct_type_specifier()?),

                // Type qualifiers
                Token::Keyword(Keyword::Const) => DeclarationSpecifier::TypeQualifier(TypeQualifier::Const),
                Token::Keyword(Keyword::Restrict) => DeclarationSpecifier::TypeQualifier(TypeQualifier::Restrict),
                Token::Keyword(Keyword::Volatile) => DeclarationSpecifier::TypeQualifier(TypeQualifier::Volatile),

                // Function specifiers
                // Some(span!(Token::Keyword(Keyword::Inline))) => DeclarationSpecifier::Function(FunctionSpecifier::Inline),
                x => unimplemented!("{:#?}", x),
            },
        ))
    }
    pub fn parse_init_declarators(&mut self) -> PResult<Vec<InitDeclarator>> {
        let mut init_declarators = vec![];
        while self.can_parse_init_declarator() || self.consider_comma(&init_declarators)? {
            if init_declarators.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma))?;
            }
            init_declarators.push(self.parse_init_declarator()?);
        }
        Ok(init_declarators)
    }

    pub fn can_parse_init_declarator(&mut self) -> bool {
        self.can_parse_declarator()
    }
    pub fn can_parse_declarator(&mut self) -> bool {
        return matches!(self.token_stream.peek(), Some(span!(Token::Punctuator(Punctuator::Asterisk) | Token::Identifier(_) | Token::Punctuator(Punctuator::LeftParen))));
    }
    pub fn parse_init_declarator(&mut self) -> PResult<InitDeclarator> {
        let declarator = self.parse_declarator()?;
        dbg!(&declarator);
        if self.eat_if_next(Token::Punctuator(Punctuator::Equal))? {
            // let initializer = self.parse_initializer();
            todo!()
        } else {
            Ok(InitDeclarator { declarator })
        }
    }
    pub fn parse_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        if self.can_parse_pointer()? {
            let ptr = self.parse_pointer()?;
            let declarator = self.parse_direct_declarator()?;

            Ok(Spanned::new(ptr.range.start..declarator.range.end, Declarator::Pointer(ptr, Box::new(declarator))))
        } else {
            let declarator = self.parse_direct_declarator()?;
            Ok(declarator)
        }
    }
    pub fn parse_direct_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        let base = match self.next_token()? {
            span!(span, Token::Identifier(identifier)) => Spanned::new(span, Declarator::Identifier(identifier.clone())),
            span!(span, Token::Punctuator(Punctuator::LeftParen)) => {
                let d = self.parse_declarator()?;
                self.expect_token(Token::Punctuator(Punctuator::RightParen))?;
                Spanned::new(span, Declarator::Parenthesized(Box::new(d)))
            }
            x => return Err(SimpleError(x.range, format!("Expected direct declarator, found {:#?}", x.value)).into_why_report()),
        };
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
            let params = self.parse_parameter_list()?;
            let rp = self.expect_token(Token::Punctuator(Punctuator::RightParen))?;

            let span = base.range.start..rp.range.end;

            return Ok(Spanned::new(span, Declarator::Function(Box::new(base), params)));
        }
        // arroy and function stuff here
        Ok(base)
    }
}
