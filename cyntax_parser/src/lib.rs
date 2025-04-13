use std::{ops::Range, str::FromStr};

use ast::{Declaration, DeclarationSpecifier, Declarator, ExternalDeclaration, InitDeclarator, ParameterDeclaration, Pointer, TranslationUnit, TypeQualifier};
use cyntax_common::{
    ast::{Keyword, Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;
use peekmore::{PeekMore, PeekMoreIterator};
#[macro_use]
pub mod patterns;
pub mod ast;
pub type PResult<T> = Result<T, cyntax_errors::why::Report>;

#[derive(Debug)]
struct TokenStream {
    iter: std::vec::IntoIter<Spanned<Token>>,
}
impl Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next()? {
                span!(span, Token::Identifier(identifier)) if Keyword::from_str(&identifier).is_ok() => return Some(Spanned::new(span, Token::Keyword(Keyword::from_str(&identifier).unwrap()))),
                span!(span, Token::BlueIdentifier(identifier)) => return Some(Spanned::new(span, Token::Identifier(identifier.to_string()))),
                span!(Token::Whitespace(_)) => continue,
                spanned_token => return Some(spanned_token),
            }
        }
    }
}
#[derive(Debug)]
pub struct Parser {
    token_stream: peekmore::PeekMoreIterator<TokenStream>,
    delimited_stack: Vec<PeekMoreIterator<TokenStream>>,
    last_location: Range<usize>,
}
impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Parser {
            token_stream: TokenStream { iter: tokens.into_iter() }.peekmore(),
            delimited_stack: vec![],
            last_location: 0..0,
        }
    }
    pub fn next_token(&mut self) -> PResult<Spanned<Token>> {
        match self.token_stream.next() {
            Some(token) => {
                self.last_location = token.range.clone();
                Ok(token)
            }
            None => Err(SimpleError {
                0: self.last_location.clone(),
                1: "Unexpected EOF!".to_string(),
            }
            .into_why_report()),
        }
    }
    pub fn peek_token(&mut self) -> PResult<&Spanned<Token>> {
        match self.token_stream.peek() {
            Some(token) => {
                self.last_location = token.range.clone();
                Ok(token)
            }
            None => Err(SimpleError {
                0: self.last_location.clone(),
                1: "Unexpected EOF!".to_string(),
            }
            .into_why_report()),
        }
    }
    pub fn eat_if_next(&mut self, t: Token) -> PResult<bool> {
        if self.peek_token()?.value == t {
            self.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
    }
    pub fn expect_token(&mut self, t: Token) -> PResult<()> {
        match self.next_token()? {
            span!(token) if token == t => Ok(()),
            token => Err(SimpleError(token.range, format!("expected {:?}, found {:?}", t, token.value)).into_why_report()),
        }
    }
    pub fn parse_translation_unit(&mut self) -> PResult<TranslationUnit> {
        let mut external_declarations = vec![];
        while let Some(external_declaration) = self.parse_external_declaration()? {
            external_declarations.push(external_declaration);
        }
        Ok(TranslationUnit { external_declarations })
    }
    pub fn parse_external_declaration(&mut self) -> PResult<Option<ExternalDeclaration>> {
        if self.token_stream.peek().is_none() {
            return Ok(None);
        }
        let declaration = self.parse_declaration()?;

        Ok(Some(ExternalDeclaration::Declaration(declaration)))
    }

    pub fn parse_declaration(&mut self) -> PResult<Declaration> {
        let specifiers = self.parse_declaration_specifiers()?;
        if self.eat_if_next(Token::Punctuator(Punctuator::Semicolon))? {
            Ok(Declaration { specifiers, init_declarators: vec![] })
        } else {
            let init_declarators = self.parse_init_declarators()?;
            Ok(Declaration { specifiers, init_declarators })
        }
    }
    pub fn parse_declaration_specifiers(&mut self) -> PResult<Vec<DeclarationSpecifier>> {
        let mut declaration_specifiers = vec![];
        while self.can_parse_declaration_specifier() {
            let specifier = self.parse_declaration_specifier()?;
            declaration_specifiers.push(specifier);
        }
        Ok(declaration_specifiers)
    }
    pub fn can_parse_declaration_specifier(&mut self) -> bool {
        // todo: add typename/identifiers to this
        return matches!(self.token_stream.peek(), Some(span!(Token::Keyword(storage_class!() | type_specifier!() | type_qualifier!() | Keyword::Inline))));
    }
    pub fn parse_declaration_specifier(&mut self) -> PResult<DeclarationSpecifier> {
        Ok(match self.next_token()? {
            // Storage-class specifiers
            span!(Token::Keyword(Keyword::Typedef)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Typedef),
            span!(Token::Keyword(Keyword::Extern)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Extern),
            span!(Token::Keyword(Keyword::Static)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Static),
            span!(Token::Keyword(Keyword::Auto)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Auto),
            span!(Token::Keyword(Keyword::Register)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Register),

            // Type specifiers
            span!(Token::Keyword(Keyword::Void)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Void),
            span!(Token::Keyword(Keyword::Char)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Char),
            span!(Token::Keyword(Keyword::Short)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Short),
            span!(Token::Keyword(Keyword::Int)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Int),
            span!(Token::Keyword(Keyword::Long)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Long),
            span!(Token::Keyword(Keyword::Float)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Float),
            span!(Token::Keyword(Keyword::Double)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Double),
            span!(Token::Keyword(Keyword::Signed)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Signed),
            span!(Token::Keyword(Keyword::Unsigned)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Unsigned),
            span!(Token::Keyword(Keyword::Bool)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Bool),
            span!(Token::Keyword(Keyword::Complex)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Complex),

            // Type qualifiers
            span!(Token::Keyword(Keyword::Const)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Const),
            span!(Token::Keyword(Keyword::Restrict)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Restrict),
            span!(Token::Keyword(Keyword::Volatile)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Volatile),

            // Function specifiers
            // Some(span!(Token::Keyword(Keyword::Inline))) => DeclarationSpecifier::Function(ast::FunctionSpecifier::Inline),
            x => unimplemented!("{:#?}", x),
        })
    }
    fn consider_comma<T>(&mut self, v: &Vec<T>) -> PResult<bool> {
        Ok(v.len() >= 1 && matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Comma))))
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
    pub fn parse_declarator(&mut self) -> PResult<Declarator> {
        if self.can_parse_pointer()? {
            let ptr = self.parse_pointer()?;
            let declarator = self.parse_direct_declarator()?;

            Ok(Declarator::Pointer(ptr, Box::new(declarator)))
        } else {
            let declarator = self.parse_direct_declarator()?;
            Ok(declarator)
        }
    }
    pub fn can_parse_pointer(&mut self) -> PResult<bool> {
        Ok(matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Asterisk))))
    }
    pub fn parse_pointer(&mut self) -> PResult<Pointer> {
        self.expect_token(Token::Punctuator(Punctuator::Asterisk))?;
        let type_qualifiers = self.parse_type_qualifiers()?;

        if self.can_parse_pointer()? {
            let ptr = self.parse_pointer()?;
            Ok(Pointer { type_qualifiers, ptr: Some(Box::new(ptr)) })
        } else {
            Ok(Pointer { type_qualifiers, ptr: None })
        }
    }
    pub fn parse_type_qualifiers(&mut self) -> PResult<Vec<TypeQualifier>> {
        let mut type_qualifiers = vec![];

        while self.can_parse_type_qualifier()? {
            type_qualifiers.push(self.parse_type_qualifier()?);
        }
        Ok(type_qualifiers)
    }
    pub fn can_parse_type_qualifier(&mut self) -> PResult<bool> {
        Ok(matches!(self.peek_token()?, span!(Token::Keyword(type_qualifier!()))))
    }
    pub fn parse_type_qualifier(&mut self) -> PResult<TypeQualifier> {
        Ok(match self.next_token()? {
            span!(Token::Keyword(Keyword::Const)) => ast::TypeQualifier::Const,
            span!(Token::Keyword(Keyword::Restrict)) => ast::TypeQualifier::Restrict,
            span!(Token::Keyword(Keyword::Volatile)) => ast::TypeQualifier::Volatile,
            _ => unreachable!(),
        })
    }
    pub fn parse_direct_declarator(&mut self) -> PResult<Declarator> {
        let base = match self.next_token()? {
            span!(Token::Identifier(identifier)) => Declarator::Identifier(identifier.clone()),
            span!(Token::Punctuator(Punctuator::LeftParen)) => {
                let d = self.parse_declarator()?;
                self.expect_token(Token::Punctuator(Punctuator::RightParen))?;
                Declarator::Parenthesized(Box::new(d))
            }
            x => unreachable!("{:#?}", x),
        };
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
            let params = self.parse_parameter_list()?;
            self.expect_token(Token::Punctuator(Punctuator::RightParen))?;

            return Ok(Declarator::Function(Box::new(base), params));
        }
        // arroy and function stuff here
        Ok(base)
    }
    pub fn parse_parameter_list(&mut self) -> PResult<Vec<ParameterDeclaration>> {
        let mut parameters = vec![];

        while self.can_parse_parameter() || self.consider_comma(&parameters)? {
            if parameters.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma))?;
            }
            parameters.push(self.parse_parameter()?)
        }
        Ok(parameters)
    }
    pub fn can_parse_parameter(&mut self) -> bool {
        return self.can_parse_declaration_specifier();
    }
    pub fn parse_parameter(&mut self) -> PResult<ParameterDeclaration> {
        //todo: abstract declarator
        let specifiers = self.parse_declaration_specifiers()?;
        let declarator = self.parse_declarator()?;
        Ok(ParameterDeclaration { specifiers, declarator })
    }
}
