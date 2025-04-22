use std::ops::Deref;

///Constraints
// 2 At least one type specifier shall be given in the declaration specifiers in each declaration,
// and in the specifier-qualifier list in each struct declaration and type name. Each list of
// type specifiers shall be one of the following sets (delimited by commas, when there is
// more than one set on a line); the type specifiers may occur in any order, possibly
// intermixed with the other declaration specifiers.
use crate::{PResult, Scope};
use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::{Diagnostic, Label, errors::SimpleError};
use cyntax_hir::{HirId, ParsedDeclarationSpecifiers, TyQualifiers, TypeSpecifierStateMachine};
use cyntax_parser::ast::{self, Identifier};
// 6.7.2 Type specifiers
// — void
// — char
// — signed char
// — unsigned char
// — short, signed short, short int, or signed short int
// — unsigned short, or unsigned short int
// — int, signed, or signed int
// — unsigned, or unsigned int
// — long, signed long, long int, or signed long int
// — unsigned long, or unsigned long int
// — long long, signed long long, long long int, or
// — signed long long int
// — unsigned long long, or unsigned long long int
// — float
// — double
// — long double
// — _Bool
// — float _Complex
// — double _Complex
// — long double _Complex
// — struct or union specifier ∗
// — enum specifier
// — typedef name
// #[derive(Debug)]
// pub enum DataType {
//     Void,
//     I8,
//     U8,
//     I16,
//     U16,
//     I32,
//     U32,
//     I64,
//     U64,
//     I128,
//     U128,
//     F32,
//     F64,
//     F128,
//     Bool,
//     Struct,
//     Enum,
//     Typedef(HirId),
// }


#[derive(Debug)]
pub struct DeclarationSpecifierParser<'a, I: Iterator<Item = &'a Spanned<ast::DeclarationSpecifier>>> {
    pub input: I,
    class: Option<&'a ast::StorageClassSpecifier>,
    base_type: TypeSpecifierStateMachine,
    pub qualifier: TyQualifiers,
    scopes: &'a Vec<Scope>
}
impl<'a, I: Iterator<Item = &'a Spanned<ast::DeclarationSpecifier>>> DeclarationSpecifierParser<'a, I> {
    pub fn new(input: I, scopes: &'a Vec<Scope>) -> Self {
        Self {
            input,
            class: None,
            base_type: TypeSpecifierStateMachine::None,
            qualifier: TyQualifiers { conzt: false, restrict: false, volatile: false },
            scopes
        }
    }
    pub fn parse(mut self) -> PResult<ParsedDeclarationSpecifiers> {
        let mut last_location = Location::new();
        while let Some(specifier) = self.input.next() {
            let loc = specifier.location.clone();
            last_location = loc.clone();
            match &specifier.value {
                ast::DeclarationSpecifier::StorageClass(storage_class_specifier) => match storage_class_specifier {
                    _ if self.class.is_some() => return Err(SimpleError(specifier.location.clone(), format!("already have a storage class")).into_codespan_report()),
                    class => self.class = Some(class),
                },
                ast::DeclarationSpecifier::TypeSpecifier(type_specifier) => match type_specifier {
                    ast::TypeSpecifier::Void => self.base_type = self.base_type.void(loc)?,
                    ast::TypeSpecifier::Char => self.base_type = self.base_type.char(loc)?,
                    ast::TypeSpecifier::Short => self.base_type = self.base_type.short(loc)?,
                    ast::TypeSpecifier::Int => self.base_type = self.base_type.int(loc)?,
                    ast::TypeSpecifier::Long => self.base_type = self.base_type.long(loc)?,
                    ast::TypeSpecifier::Float => self.base_type = self.base_type.float(loc)?,
                    ast::TypeSpecifier::Double => self.base_type = self.base_type.double(loc)?,
                    ast::TypeSpecifier::Signed => self.base_type = self.base_type.signed(loc)?,
                    ast::TypeSpecifier::Unsigned => self.base_type = self.base_type.unsigned(loc)?,
                    ast::TypeSpecifier::Bool => self.base_type = self.base_type.bool(loc)?,
                    ast::TypeSpecifier::TypedefName(typedef_name) =>{
                        let t = self.find_typedef_in_scope(&loc, typedef_name)?;

                        self.base_type = self.base_type.typedef_name(loc, t)?;
                    }
                    x => unimplemented!("{x:?}"),
                },
                ast::DeclarationSpecifier::TypeQualifier(type_qualifier) => match type_qualifier {
                    ast::TypeQualifier::Const => self.qualifier.conzt = true,
                    ast::TypeQualifier::Restrict => self.qualifier.restrict = true,
                    ast::TypeQualifier::Volatile => self.qualifier.volatile = true,
                },
                ast::DeclarationSpecifier::FunctionSpecifier(function_specifier) => todo!(),
            }
        }
        if let TypeSpecifierStateMachine::None = self.base_type {
            return Err(SimpleError(last_location, format!("must have at least 1 type specifier")).into_codespan_report());
        }

        Ok(ParsedDeclarationSpecifiers {
            class: self.class.cloned(),
            specifiers: self.base_type,
            qualifier: self.qualifier,
        })
    }
    fn find_typedef_in_scope(&mut self, loc: &Location, identifier: &Identifier) -> PResult<HirId> {
        // reversed, because the newest scope has priority over the 2nd newest
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.typedefs.get(&identifier) {
                return Ok(id);
            }
        }
        Err(SimpleError(loc.clone(), format!("could not find typedef in scope")).into_codespan_report())
    }
}