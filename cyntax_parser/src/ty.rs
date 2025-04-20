use cyntax_common::ast::{Keyword, Punctuator};
use cyntax_common::spanned::Spanned;
use cyntax_errors::errors::SimpleWarning;
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_common::span;

use crate::{
    PResult, Parser,
    ast::{self, EnumDeclaration, EnumSpecifier, ParameterDeclaration, Pointer, SpecifierQualifier, StructOrUnionDeclaration, StructDeclarator, StructOrUnionSpecifier, Token, TypeQualifier, TypeSpecifier},
};

impl<'src> Parser<'src> {
    pub fn parse_struct_or_union_type_specifier(&mut self, is_union: bool) -> PResult<ast::TypeSpecifier> {
        let tag = if let span!(Token::Identifier(identifer)) = self.peek_token()? { Some(identifer.clone()) } else { None };
        if tag.is_some() {
            self.next_token()?;
        }

        if self.eat_if_next(Token::Punctuator(Punctuator::LeftBrace))? {
            let declarations = self.parse_struct_declaration_list()?;

            self.expect_token(Token::Punctuator(Punctuator::RightBrace), "to close struct type specifier")?;
            dbg!(&self.peek_token());
            if is_union {
                Ok(ast::TypeSpecifier::Union(StructOrUnionSpecifier { tag, declarations: vec![] }))
            } else {
                Ok(ast::TypeSpecifier::Union(StructOrUnionSpecifier { tag, declarations }))
            }
        } else {
            if is_union {
                Ok(ast::TypeSpecifier::Union(StructOrUnionSpecifier { tag, declarations: vec![] }))
            } else {
                Ok(ast::TypeSpecifier::Union(StructOrUnionSpecifier { tag, declarations: vec![] }))
            }
        }
    }

    pub fn parse_struct_declaration_list(&mut self) -> PResult<Vec<StructOrUnionDeclaration>> {
        let mut struct_declarations = vec![];
        // because i use recoverable parsing on the inside of this, its fine to not check the current token
        while !matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::RightBrace)))) {
            if let Some((specifier_qualifiers, declarators)) = self.maybe_recover(
                |this| {
                    let specifier_qualifiers = this.parse_specifier_qualifier_list()?;
                    dbg!(&specifier_qualifiers);
                    if !specifier_qualifiers.iter().any(|sq| matches!(sq, SpecifierQualifier::Specifier(_))) {
                        return Err(SimpleError(this.last_location.clone(), "Struct declarations must have atleast one specifier".to_string()).into_codespan_report());
                    }
                    let declarators = this.parse_struct_declarator_list()?;
                    this.expect_token(Token::Punctuator(Punctuator::Semicolon), "to end a struct declaration")?;

                    Ok(Some((specifier_qualifiers, declarators)))
                },
                |_| None,
                Token::Punctuator(Punctuator::Semicolon),
            ) {
                struct_declarations.push(StructOrUnionDeclaration { declarators, specifier_qualifiers });
            }
        }

        Ok(struct_declarations)
    }
    pub fn parse_enum_type_specifier(&mut self) -> PResult<ast::TypeSpecifier> {
        let name = if let span!(Token::Identifier(identifer)) = self.peek_token()? { Some(identifer.clone()) } else { None };
        if name.is_some() {
            self.next_token()?;
        }
        if self.eat_if_next(Token::Punctuator(Punctuator::LeftBrace))? {
            let declarations = self.parse_enum_declaration_list()?;

            // if let Some(span!(loc, _)) = self.eat_next(Token::Punctuator(Punctuator::Comma))? {
            //     self.next_token().unwrap();
            //     self.diagnostics.push(SimpleWarning(loc, "trailing comma".to_string()).into_codespan_report());
            // }
            Ok(ast::TypeSpecifier::Enum(EnumSpecifier { identifier: name, declarations }))
        } else {
            Ok(ast::TypeSpecifier::Enum(EnumSpecifier { identifier: name, declarations: vec![] }))
        }
    }
    pub fn parse_enum_declaration_list(&mut self) -> PResult<Vec<EnumDeclaration>> {
        let mut enum_declarations = vec![];
        while !self.eat_if_next(Token::Punctuator(Punctuator::RightBrace))? {
            if enum_declarations.len() > 0 {
                let comma = self.expect_token(Token::Punctuator(Punctuator::Comma), "expected comma between enum declarations")?;

                // recover from trailing comma
                if self.eat_if_next(Token::Punctuator(Punctuator::RightBrace))? {
                    self.diagnostics.push(SimpleWarning(comma.location, "trailing comma".to_string()).into_codespan_report());
                    break;
                }
            }
            let identifier = self.expect_non_typename_identifier()?;
            let expression = if self.eat_if_next(Token::Punctuator(Punctuator::Equal))? { Some(self.parse_expression()?) } else { None };

            enum_declarations.push(EnumDeclaration { identifier: identifier, value: expression })
        }
        Ok(enum_declarations)
    }
    pub fn parse_specifier_qualifier_list(&mut self) -> PResult<Vec<SpecifierQualifier>> {
        let mut specifier_qualifiers = vec![];
        while self.can_parse_type_qualifier() || self.can_parse_type_specifier() {
            specifier_qualifiers.push(match self.next_token()? {
                span!(Token::Keyword(Keyword::Void)) => SpecifierQualifier::Specifier(TypeSpecifier::Void),
                span!(Token::Keyword(Keyword::Char)) => SpecifierQualifier::Specifier(TypeSpecifier::Char),
                span!(Token::Keyword(Keyword::Short)) => SpecifierQualifier::Specifier(TypeSpecifier::Short),
                span!(Token::Keyword(Keyword::Int)) => SpecifierQualifier::Specifier(TypeSpecifier::Int),
                span!(Token::Keyword(Keyword::Long)) => SpecifierQualifier::Specifier(TypeSpecifier::Long),
                span!(Token::Keyword(Keyword::Float)) => SpecifierQualifier::Specifier(TypeSpecifier::Float),
                span!(Token::Keyword(Keyword::Double)) => SpecifierQualifier::Specifier(TypeSpecifier::Double),
                span!(Token::Keyword(Keyword::Signed)) => SpecifierQualifier::Specifier(TypeSpecifier::Signed),
                span!(Token::Keyword(Keyword::Unsigned)) => SpecifierQualifier::Specifier(TypeSpecifier::Unsigned),
                span!(Token::Keyword(Keyword::Bool)) => SpecifierQualifier::Specifier(TypeSpecifier::Bool),
                span!(Token::Keyword(Keyword::Complex)) => SpecifierQualifier::Specifier(TypeSpecifier::Complex),
                span!(Token::Keyword(Keyword::Struct)) => SpecifierQualifier::Specifier(self.parse_struct_or_union_type_specifier(false)?),
                span!(Token::Keyword(Keyword::Union)) => SpecifierQualifier::Specifier(self.parse_struct_or_union_type_specifier(true)?),

                span!(Token::Keyword(Keyword::Enum)) => SpecifierQualifier::Specifier(self.parse_enum_type_specifier()?),

                span!(Token::Identifier(identifier)) if self.is_typedef(&identifier) => SpecifierQualifier::Specifier(TypeSpecifier::TypedefName(identifier)),
                span!(Token::Keyword(kw @ type_qualifier!())) => SpecifierQualifier::Qualifier(kw.into()),
                _ => unreachable!(),
            });
        }
        Ok(specifier_qualifiers)
    }
    pub fn parse_struct_declarator_list(&mut self) -> PResult<Vec<StructDeclarator>> {
        let mut declarators = vec![];
        while self.can_start_declarator() || self.consider_comma(&declarators)? {
            if declarators.len() > 0 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate declarators in struct declaration")?;
            }
            let declarator = self.parse_declarator()?;
            declarators.push(StructDeclarator { declarator: Some(declarator) });
        }
        Ok(declarators)
    }

    pub fn can_start_pointer(&mut self) -> PResult<bool> {
        Ok(matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Asterisk))))
    }
    pub fn parse_pointer(&mut self) -> PResult<Spanned<Pointer>> {
        let asterisk = self.expect_token(Token::Punctuator(Punctuator::Asterisk), "for pointer")?;
        let type_qualifiers = self.parse_type_qualifiers()?;

        if self.can_start_pointer()? {
            let ptr = self.parse_pointer()?;
            Ok(Spanned::new(asterisk.location.until(&ptr.location), Pointer { type_qualifiers, ptr: Some(Box::new(ptr)) }))
        } else {
            let range = asterisk.location.as_fallback_for_vec(&type_qualifiers);
            Ok(Spanned::new(range, Pointer { type_qualifiers, ptr: None }))
        }
    }
    pub fn parse_type_qualifiers(&mut self) -> PResult<Vec<Spanned<TypeQualifier>>> {
        let mut type_qualifiers = vec![];

        while self.can_parse_type_qualifier() {
            type_qualifiers.push(self.parse_type_qualifier()?);
        }
        Ok(type_qualifiers)
    }
    pub fn can_parse_type_qualifier(&mut self) -> bool {
        matches!(self.peek_token(), Ok(span!(Token::Keyword(type_qualifier!()))))
    }
    pub fn can_parse_type_specifier(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Keyword(type_specifier!()))) => true,
            Ok(span!(Token::Identifier(identifier))) if self.is_typedef(&identifier) => true,
            _ => false,
        }
    }
    pub fn parse_type_qualifier(&mut self) -> PResult<Spanned<TypeQualifier>> {
        let Spanned { value, location } = self.next_token()?;

        Ok(Spanned::new(
            location,
            match value {
                Token::Keyword(Keyword::Const) => ast::TypeQualifier::Const,
                Token::Keyword(Keyword::Restrict) => ast::TypeQualifier::Restrict,
                Token::Keyword(Keyword::Volatile) => ast::TypeQualifier::Volatile,
                _ => unreachable!(),
            },
        ))
    }

    pub fn parse_parameter_list(&mut self) -> PResult<Vec<Spanned<ParameterDeclaration>>> {
        let mut parameters = vec![];

        while self.can_parse_parameter() || self.consider_comma(&parameters)? {
            if parameters.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate parameters")?;
            }
            parameters.push(self.parse_parameter_declaration(false)?)
        }
        Ok(parameters)
    }
    pub fn parse_parameter_type_list(&mut self) -> PResult<Vec<Spanned<ParameterDeclaration>>> {
        let mut parameters = vec![];

        while self.can_parse_parameter() || self.consider_comma(&parameters)? {
            if parameters.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate parameters")?;
            }
            parameters.push(self.parse_parameter_declaration(true)?)
        }
        Ok(parameters)
    }
    pub fn can_parse_parameter(&mut self) -> bool {
        return self.can_start_declaration_specifier();
    }
    pub fn parse_parameter_declaration(&mut self, allow_abstract: bool) -> PResult<Spanned<ParameterDeclaration>> {
        let start = self.last_location.clone();
        let specifiers = self.parse_declaration_specifiers()?;
        let declarator = if self.can_start_declarator() { Some(self.parse_declarator()?) } else { None };
        if let Some(declarator) = &declarator {
            self.declare_identifier(declarator)?;
        } 
        if declarator.is_none() && !allow_abstract {
            return Err(SimpleError(start, "this declaration does not allow abstract declarators".to_string()).into_codespan_report());
        }

        let range = start.as_fallback_for_vec(&specifiers);
        Ok(Spanned::new(range, ParameterDeclaration { specifiers, declarator: declarator }))
    }
}
