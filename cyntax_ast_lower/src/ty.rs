///Constraints
// 2 At least one type specifier shall be given in the declaration specifiers in each declaration,
// and in the specifier-qualifier list in each struct declaration and type name. Each list of
// type specifiers shall be one of the following sets (delimited by commas, when there is
// more than one set on a line); the type specifiers may occur in any order, possibly
// intermixed with the other declaration specifiers.
use crate::PResult;
use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::{Diagnostic, Label, errors::SimpleError};
use cyntax_hir::HirId;
use cyntax_parser::ast;
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
#[derive(Debug)]
pub enum TySpecifier {
    Void,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    F128,
    Bool,
    Struct,
    Enum,
    Typedef(HirId),
}

#[derive(Debug)]
#[rustfmt::skip]
enum TypeSpecifierStateMachine {
    None,
    Void,
    Char,
    SignedChar,
    UnsignedChar,
    Short, SignedShort, ShortInt, SignedShortInt,
    UnsignedShort, UnsignedShortInt,
    Int, Signed, SignedInt,
    Unsigned, UnsignedInt,
    Long, SignedLong, LongInt, SignedLongInt,
    UnsignedLong, UnsignedLongInt,
    LongLong, SignedLongLong, LongLongInt, SignedLongLongInt, UnsignedLongLong, UnsignedLongLongInt,
    Float,
    Double,
    LongDouble,
    StructOrUnion,
    Enum,
    Typedef,
    Bool,
    FloatComplex,
    DoubleComplex,
    LongDoubleComplex,
}

#[derive(Debug)]
pub struct DeclarationSpecifierParser<'a> {
    pub input: std::slice::Iter<'a, Spanned<ast::DeclarationSpecifier>>,
    class: Option<&'a ast::StorageClassSpecifier>,
    base_type: TypeSpecifierStateMachine,
    qualifier: TyQualifiers,
}
#[derive(Debug)]
pub struct TyQualifiers {
    conzt: bool,
    restrict: bool,
    volatile: bool,
}
#[derive(Debug)]
pub struct ParsedDeclarationSpecifiers {
    class: Option<ast::StorageClassSpecifier>,
    specifiers: TypeSpecifierStateMachine,
    qualifier: TyQualifiers,
}
impl<'a> DeclarationSpecifierParser<'a> {
    pub fn new(input: std::slice::Iter<'a, Spanned<ast::DeclarationSpecifier>>) -> Self {
        Self {
            input,
            class: None,
            base_type: TypeSpecifierStateMachine::None,
            qualifier: TyQualifiers { conzt: false, restrict: false, volatile: false },
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
                    _ => unimplemented!(),
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
}
impl<'a> Diagnostic for TypeSpecifierStateMachineError<'a> {
    fn title<'b>(&self) -> &'b str {
        "error while parsing type specifiers"
    }

    fn severity(&self) -> cyntax_errors::DiagnosticSeverity {
        cyntax_errors::DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<cyntax_errors::Label> {
        match self {
            TypeSpecifierStateMachineError::InvalidTransition(state, location, transition) => {
                vec![cyntax_errors::Label {
                    kind: cyntax_errors::LabelKind::Primary,
                    location: location.clone(),
                    message: format!("{} is not a valid transition for state {:?}", transition, state),
                }]
            }
        }
    }
}

enum TypeSpecifierStateMachineError<'a> {
    InvalidTransition(&'a TypeSpecifierStateMachine, Location, &'a str),
}

impl TypeSpecifierStateMachine {
    pub fn void(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Void),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "void").into_codespan_report()),
        }
    }

    pub fn char(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Char),
            TypeSpecifierStateMachine::Signed => Ok(Self::SignedChar),
            TypeSpecifierStateMachine::Unsigned => Ok(Self::UnsignedChar),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "char").into_codespan_report()),
        }
    }

    pub fn signed(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Signed),
            TypeSpecifierStateMachine::Char => Ok(Self::SignedChar),
            TypeSpecifierStateMachine::Short => Ok(Self::SignedShort),
            TypeSpecifierStateMachine::ShortInt => Ok(Self::SignedShortInt),
            TypeSpecifierStateMachine::Int => Ok(Self::SignedInt),
            TypeSpecifierStateMachine::Long => Ok(Self::SignedLong),
            TypeSpecifierStateMachine::LongInt => Ok(Self::SignedLongInt),
            TypeSpecifierStateMachine::LongLong => Ok(Self::SignedLongLong),
            TypeSpecifierStateMachine::LongLongInt => Ok(Self::SignedLongLongInt),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "signed").into_codespan_report()),
        }
    }

    pub fn unsigned(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Unsigned),
            TypeSpecifierStateMachine::Char => Ok(Self::UnsignedChar),
            TypeSpecifierStateMachine::Short => Ok(Self::UnsignedShort),
            TypeSpecifierStateMachine::ShortInt => Ok(Self::UnsignedShortInt),
            TypeSpecifierStateMachine::Int => Ok(Self::UnsignedInt),
            TypeSpecifierStateMachine::Long => Ok(Self::UnsignedLong),
            TypeSpecifierStateMachine::LongInt => Ok(Self::UnsignedLongInt),
            TypeSpecifierStateMachine::LongLong => Ok(Self::UnsignedLongLong),
            TypeSpecifierStateMachine::LongLongInt => Ok(Self::UnsignedLongLongInt),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "unsigned").into_codespan_report()),
        }
    }

    pub fn short(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Short),
            TypeSpecifierStateMachine::Signed => Ok(Self::SignedShort),
            TypeSpecifierStateMachine::Unsigned => Ok(Self::UnsignedShort),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "short").into_codespan_report()),
        }
    }

    pub fn int(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Int),
            TypeSpecifierStateMachine::Signed => Ok(Self::SignedInt),
            TypeSpecifierStateMachine::Unsigned => Ok(Self::UnsignedInt),
            TypeSpecifierStateMachine::Short => Ok(Self::ShortInt),
            TypeSpecifierStateMachine::SignedShort => Ok(Self::SignedShortInt),
            TypeSpecifierStateMachine::UnsignedShort => Ok(Self::UnsignedShortInt),
            TypeSpecifierStateMachine::Long => Ok(Self::LongInt),
            TypeSpecifierStateMachine::SignedLong => Ok(Self::SignedLongInt),
            TypeSpecifierStateMachine::UnsignedLong => Ok(Self::UnsignedLongInt),
            TypeSpecifierStateMachine::LongLong => Ok(Self::LongLongInt),
            TypeSpecifierStateMachine::SignedLongLong => Ok(Self::SignedLongLongInt),
            TypeSpecifierStateMachine::UnsignedLongLong => Ok(Self::UnsignedLongLongInt),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "int").into_codespan_report()),
        }
    }

    pub fn long(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Long),
            TypeSpecifierStateMachine::Signed => Ok(Self::SignedLong),
            TypeSpecifierStateMachine::Unsigned => Ok(Self::UnsignedLong),
            TypeSpecifierStateMachine::Int => Ok(Self::LongInt),
            TypeSpecifierStateMachine::SignedInt => Ok(Self::SignedLongInt),
            TypeSpecifierStateMachine::UnsignedInt => Ok(Self::UnsignedLongInt),
            TypeSpecifierStateMachine::Long => Ok(Self::LongLong),
            TypeSpecifierStateMachine::SignedLong => Ok(Self::SignedLongLong),
            TypeSpecifierStateMachine::UnsignedLong => Ok(Self::UnsignedLongLong),
            TypeSpecifierStateMachine::LongInt => Ok(Self::LongLongInt),
            TypeSpecifierStateMachine::SignedLongInt => Ok(Self::SignedLongLongInt),
            TypeSpecifierStateMachine::UnsignedLongInt => Ok(Self::UnsignedLongLongInt),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "long").into_codespan_report()),
        }
    }

    pub fn float(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Float),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "float").into_codespan_report()),
        }
    }

    pub fn double(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Double),
            TypeSpecifierStateMachine::Long => Ok(Self::LongDouble),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "double").into_codespan_report()),
        }
    }

    pub fn struct_or_union(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::StructOrUnion),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "struct").into_codespan_report()),
        }
    }

    pub fn enum_specifier(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Enum),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "enum").into_codespan_report()),
        }
    }

    pub fn typedef_name(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Typedef),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "typedef").into_codespan_report()),
        }
    }

    pub fn bool(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Bool),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "bool").into_codespan_report()),
        }
    }

    pub fn complex(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::Float => Ok(Self::FloatComplex),
            TypeSpecifierStateMachine::Double => Ok(Self::DoubleComplex),
            TypeSpecifierStateMachine::LongDouble => Ok(Self::LongDoubleComplex),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "_complex").into_codespan_report()),
        }
    }
}
