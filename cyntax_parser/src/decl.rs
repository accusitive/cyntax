use crate::ast::*;
use crate::{PResult, Parser};
use cyntax_common::ast::*;
use cyntax_common::spanned::Spanned;
use cyntax_errors::Diagnostic;
use cyntax_errors::errors::SimpleError;
use cyntax_lexer::span;
/// Declarations, Declarators, Init declarators, stuff of that sort
impl<'src> Parser<'src> {
    pub fn parse_external_declaration(&mut self) -> PResult<Option<ExternalDeclaration>> {
        if self.token_stream.peek().is_none() {
            return Ok(None);
        }
        let specifiers = self.parse_declaration_specifiers()?;

        let mut init_declarators = self.parse_init_declarators()?;
        let mut is_typedef = false;
        for specifier in &specifiers {
            if matches!(specifier, span!(DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef))) {
                is_typedef = true;
            }
        }
        if is_typedef {
            for declarator in &init_declarators {
                self.declare_typedef(declarator);
            }
        }

        // int main() {}
        if init_declarators.len() == 1 && init_declarators[0].initializer.is_none() && self.peek_matches(Token::Punctuator(Punctuator::LeftBrace))? {
            let declarator = init_declarators.remove(0).declarator;
            let body = self.parse_statement()?;
            return Ok(Some(ExternalDeclaration::FunctionDefinition(FunctionDefinition { specifiers, declarator, body })));
        }
        // int a,b() {}
        if init_declarators.len() > 1 && self.peek_matches(Token::Punctuator(Punctuator::LeftBrace))? {
            let range = self.last_location.as_fallback_for_vec(&specifiers).until(&self.last_location);
            return Err(SimpleError(range, "Declarations with more than 1 declarator cannot have a function body".to_string()).into_codespan_report());
        }
        // int a;
        self.expect_token(Token::Punctuator(Punctuator::Semicolon), "to end declaration")?;
        return Ok(Some(ExternalDeclaration::Declaration(Declaration { specifiers, init_declarators })));
    }
    pub fn parse_declaration(&mut self) -> PResult<Declaration> {
        let specifiers = self.parse_declaration_specifiers()?;

        let mut is_typedef = false;

        let init_declarators = self.parse_init_declarators()?;

        // self.expect_token(Token::Punctuator(Punctuator::Semicolon), "after declaration")?;
        for specifier in &specifiers {
            if matches!(specifier, span!(DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef))) {
                is_typedef = true;
            }
        }
        if is_typedef {
            for declarator in &init_declarators {
                self.declare_typedef(declarator);
            }
        }

        Ok(Declaration { specifiers, init_declarators })
    }
    pub fn parse_declaration_specifiers(&mut self) -> PResult<Vec<Spanned<DeclarationSpecifier>>> {
        let mut declaration_specifiers = vec![];
        while self.can_start_declaration_specifier() {
            let specifier = self.parse_declaration_specifier()?;
            declaration_specifiers.push(specifier);
        }
        Ok(declaration_specifiers.into())
    }
    pub fn can_start_declaration_specifier(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Keyword(storage_class!() | type_specifier!() | type_qualifier!() | Keyword::Inline))) => true,
            Ok(span!(Token::Identifier(identifier))) if self.is_typedef(&identifier) => true,
            _ => false,
        }
    }
    pub fn parse_declaration_specifier(&mut self) -> PResult<Spanned<DeclarationSpecifier>> {
        let Spanned { value, location } = self.next_token()?;
        Ok(Spanned::new(
            location,
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
                Token::Identifier(identifier) if self.is_typedef(&identifier) => DeclarationSpecifier::TypeSpecifier(TypeSpecifier::TypedefName(identifier)),
                // Function specifiers
                // Some(span!(Token::Keyword(Keyword::Inline))) => DeclarationSpecifier::Function(FunctionSpecifier::Inline),
                x => unimplemented!("{:#?}", x),
            },
        ))
    }
    pub fn parse_init_declarators(&mut self) -> PResult<Vec<InitDeclarator>> {
        let mut init_declarators = vec![];
        while self.can_start_init_declarator() || self.consider_comma(&init_declarators)? {
            if init_declarators.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate init declarators")?;
            }
            init_declarators.push(self.parse_init_declarator()?);
        }
        Ok(init_declarators)
    }

    pub fn can_start_init_declarator(&mut self) -> bool {
        self.can_start_declarator()
    }
    pub fn can_start_declarator(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::Asterisk) | Token::Identifier(_) | Token::Punctuator(Punctuator::LeftParen))));
    }
    pub fn parse_init_declarator(&mut self) -> PResult<InitDeclarator> {
        let declarator = self.parse_declarator()?;
        dbg!(&declarator);
        if self.eat_if_next(Token::Punctuator(Punctuator::Assign))? {
            let initializer = self.parse_initializer()?;
            dbg!(&initializer);
            Ok(InitDeclarator { declarator, initializer: Some(initializer) })
        } else {
            Ok(InitDeclarator { declarator, initializer: None })
        }
    }
    pub fn parse_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        if self.can_start_pointer()? {
            let ptr = self.parse_pointer()?;
            let declarator = self.parse_direct_declarator()?;

            Ok(Spanned::new(ptr.location.until(&declarator.location), Declarator::Pointer(ptr, Box::new(declarator))))
        } else {
            let declarator = self.parse_direct_declarator()?;
            Ok(declarator)
        }
    }

    pub fn parse_direct_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        dbg!();
        let base = match self.next_token()? {
            span!(span, Token::Identifier(identifier)) => Spanned::new(span, Declarator::Identifier(identifier.clone())),
            span!(span, Token::Punctuator(Punctuator::LeftParen)) => {
                let d = self.parse_declarator()?;
                self.expect_token(Token::Punctuator(Punctuator::RightParen), "to end a direct declarator")?;
                Spanned::new(span, Declarator::Parenthesized(Box::new(d)))
            }
            x => return Err(SimpleError(x.location, format!("Expected direct declarator, found {:?}", x.value)).into_codespan_report()),
        };
        dbg!(&base, self.peek_token());
        // Function stuff
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
            let params = self.parse_parameter_type_list()?;
            let rp = self.expect_token(Token::Punctuator(Punctuator::RightParen), "to end a function declarator")?;
            let span = base.location.until(&rp.location);

            return Ok(Spanned::new(span, Declarator::Function(Box::new(base), params)));
        }
        // todo: Array staff
        Ok(base)
    }
    pub fn parse_abstract_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        if self.can_start_pointer()? {
            let ptr = self.parse_pointer()?;
            let declarator = self.parse_direct_abstract_declarator()?;

            Ok(Spanned::new(ptr.location.until(&declarator.location), Declarator::Pointer(ptr, Box::new(declarator))))
        } else {
            let declarator = self.parse_direct_abstract_declarator()?;
            Ok(declarator)
        }
    }
    pub fn parse_direct_abstract_declarator(&mut self) -> PResult<Spanned<Declarator>> {
        let start = self.last_location.clone();

        let mut base = if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
            Spanned::new(start.until(&self.last_location), Declarator::Parenthesized(Box::new(self.parse_abstract_declarator()?)))
        } else {
            Spanned::new(start.until(&self.last_location), Declarator::Abstract)
        };

        // Function stuff
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
            let params = self.parse_parameter_type_list()?;
            let rp = self.expect_token(Token::Punctuator(Punctuator::RightParen), "to end an abstract function declarator")?;
            let span = start.until(&rp.location);

            base = Spanned::new(span, Declarator::Function(Box::new(base), params));
        }
        // todo: Array staff
        Ok(base)
    }
    pub fn parse_initializer(&mut self) -> PResult<Initializer> {
        //todo: assignement-expr
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftBrace))? {
            let list = self.parse_initializer_list()?;
            // comma is optional
            let _ = self.eat_if_next(Token::Punctuator(Punctuator::Comma));
            self.expect_token(Token::Punctuator(Punctuator::RightBrace), "to end an initializer")?;
            Ok(Initializer::List(list))
        } else {
            self.next_token()?;
            Ok(Initializer::Assignemnt)
        }
    }
    pub fn parse_initializer_list(&mut self) -> PResult<Vec<DesignatedIntiializer>> {
        let mut initializers = vec![];

        while self.can_start_designation() || self.can_start_initializer() || self.consider_comma(&initializers)? {
            if initializers.len() > 0 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to sperate initializers")?;
            }
            let designation = self.parse_designation()?;
            let init = self.parse_initializer()?;
            dbg!(&init);
            initializers.push(DesignatedIntiializer { designation, initializer: init })
        }
        Ok(initializers)
    }
    pub fn can_start_designation(&mut self) -> bool {
        matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::LeftBracket | Punctuator::Dot))))
    }
    pub fn can_start_initializer(&mut self) -> bool {
        matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::LeftBrace)))) || self.can_start_primary_expression()
    }
    pub fn parse_designation(&mut self) -> PResult<Vec<Designator>> {
        let mut designator_list = vec![];

        while self.can_start_designation() {
            designator_list.push(self.parse_designator()?)
        }

        if designator_list.len() > 0 {
            self.expect_token(Token::Punctuator(Punctuator::Assign), "for designation")?;
        }
        Ok(designator_list)
    }
    pub fn parse_designator(&mut self) -> PResult<Designator> {
        if self.eat_if_next(Token::Punctuator(Punctuator::Dot))? {
            let identifier = self.expect_identifier()?;
            Ok(Designator::Identifier(identifier))
        } else if self.eat_if_next(Token::Punctuator(Punctuator::LeftBracket))? {
            unimplemented!("need to implement expression parsing for this")
        } else {
            unreachable!()
        }
    }
}
