use std::fmt::Display;

use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::Diagnostic;
use cyntax_parser::{ast::{self, TypeSpecifier}, PResult};
pub type HirId = usize;

#[derive(Debug)]
pub struct TranslationUnit<'hir> {
    pub declarations: &'hir [&'hir ExternalDeclaration<'hir>],
}
#[derive(Debug)]
pub enum ExternalDeclaration<'hir> {
    FunctionDefinition(FunctionDefinition<'hir>),
    Declaration(&'hir Declaration<'hir>),
}
#[derive(Debug)]
pub struct FunctionDefinition<'hir> {
    pub body: &'hir Statement<'hir>,
}
#[derive(Debug)]
pub struct Declaration<'hir> {
    pub id: HirId,
    pub full_location: Location,
    pub declarator_loc: Location,
    pub init: Option<&'hir Initializer<'hir>>,
    pub ty: &'hir Ty<'hir>,
}
#[derive(Debug)]
pub struct StructType<'hir> {
    pub id: HirId,
    pub tag: Option<Spanned<ast::Identifier>>,
    pub kind: StructTypeKind<'hir>,
}
#[derive(Debug)]
pub enum StructTypeKind<'hir> {
    Incomplete,
    Complete(&'hir [&'hir StructField<'hir>]),
}
#[derive(Debug)]
pub struct StructField<'hir> {
    pub identifier: Option<ast::Identifier>,
    pub ty: &'hir Ty<'hir>,
}
#[derive(Debug)]
pub enum Initializer<'hir> {
    Assignment(&'hir Expression<'hir>),
}
#[derive(Debug)]
pub struct Expression<'hir> {
    pub id: HirId,
    pub loc: Location,
    pub kind: ExpressionKind<'hir>,
}
#[derive(Debug)]
pub enum ExpressionKind<'hir> {
    Constant(Spanned<cyntax_parser::constant::IntConstant>),
    BinaryOp(Spanned<ast::InfixOperator>, &'hir Expression<'hir>, &'hir Expression<'hir>),
    DeclarationReference(HirId),
    Cast(&'hir Ty<'hir>, &'hir Expression<'hir>),
    MemberAccess(&'hir Expression<'hir>, Spanned<ast::Identifier>)
}
#[derive(Debug)]
pub struct Statement<'hir> {
    pub id: usize,
    pub span: Location,
    pub kind: StatementKind<'hir>,
}
#[derive(Debug)]
pub enum StatementKind<'hir> {
    Compound(&'hir [BlockItem<'hir>]),
    Expression(&'hir Expression<'hir>),
    Return(Option<&'hir Expression<'hir>>),
    While(&'hir Expression<'hir>, &'hir Statement<'hir>),
    Continue,
    Break,
    IfThen(&'hir Expression<'hir>, &'hir Statement<'hir>),
    IfThenElse(&'hir Expression<'hir>, &'hir Statement<'hir>, &'hir Statement<'hir>),
}
#[derive(Debug)]
pub enum BlockItem<'hir> {
    Declaration(&'hir Declaration<'hir>),
    Statement(&'hir Statement<'hir>),
}

#[derive(Debug, Clone)]
pub struct TyQualifiers {
    pub conzt: bool,
    pub restrict: bool,
    pub volatile: bool,
}
#[derive(Debug, Clone)]
pub struct ParsedDeclarationSpecifiers {
    pub class: Option<ast::StorageClassSpecifier>,
    pub specifiers: TypeSpecifierStateMachine,
    pub qualifier: TyQualifiers,
}
#[derive(Debug, Clone)]
pub struct SpecifierQualifiers {
    pub specifiers: TypeSpecifierStateMachine,
    pub qualifier: TyQualifiers,
}
#[derive(Debug)]
pub struct Ty<'hir> {
    pub id: HirId,
    pub kind: TyKind<'hir>,
}
#[derive(Debug, Clone)]
pub enum TyKind<'hir> {
    Base(SpecifierQualifiers),
    Pointer(Vec<Spanned<ast::TypeQualifier>>, Box<TyKind<'hir>>),
    Function { return_ty: Box<TyKind<'hir>>, parameters: &'hir [FunctionParameter<'hir>] },
    Array(Box<Self>, &'hir Expression<'hir>),
}

#[derive(Debug)]
pub struct FunctionParameter<'hir> {
    pub ty: &'hir Ty<'hir>,
    pub identifier: Option<Spanned<ast::Identifier>>,
}
#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum TypeSpecifierStateMachine {
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
    StructOrUnion(HirId),
    Enum,
    Typedef(HirId),
    Bool,
    FloatComplex,
    DoubleComplex,
    LongDoubleComplex,
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
impl From<ParsedDeclarationSpecifiers> for SpecifierQualifiers {
    fn from(value: ParsedDeclarationSpecifiers) -> Self {
        Self {
            specifiers: value.specifiers,
            qualifier: value.qualifier,
        }
    }
}
impl From<SpecifierQualifiers> for ParsedDeclarationSpecifiers {
    fn from(value: SpecifierQualifiers) -> Self {
        Self {
            specifiers: value.specifiers,
            qualifier: value.qualifier,
            class: None,
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

    pub fn struct_or_union(&self, loc: Location, struc: HirId) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::StructOrUnion(struc)),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "struct").into_codespan_report()),
        }
    }

    pub fn enum_specifier(&self, loc: Location) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Enum),
            _ => Err(TypeSpecifierStateMachineError::InvalidTransition(self, loc, "enum").into_codespan_report()),
        }
    }

    pub fn typedef_name(&self, loc: Location, typedef: HirId) -> PResult<Self> {
        match self {
            TypeSpecifierStateMachine::None => Ok(Self::Typedef(typedef)),
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

impl<'hir> Display for Ty<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
}
impl<'hir> Display for TyKind<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::Base(SpecifierQualifiers { specifiers: TypeSpecifierStateMachine::StructOrUnion(hir_id), qualifier }) = self {
            return f.write_fmt(format_args!("StructOrUnion({})", hir_id));
        }
        match self {
            TyKind::Base(specifier_qualifiers) => f.write_fmt(format_args!(
                "{}",
                match specifier_qualifiers.specifiers {
                    TypeSpecifierStateMachine::None => "None",
                    TypeSpecifierStateMachine::Void => "Void",
                    TypeSpecifierStateMachine::Char => "Char",
                    TypeSpecifierStateMachine::SignedChar => "SignedChar",
                    TypeSpecifierStateMachine::UnsignedChar => "UnsignedChar",
                    TypeSpecifierStateMachine::Short => "Short",
                    TypeSpecifierStateMachine::SignedShort => "SignedShort",
                    TypeSpecifierStateMachine::ShortInt => "ShortInt",
                    TypeSpecifierStateMachine::SignedShortInt => "SignedShortInt",
                    TypeSpecifierStateMachine::UnsignedShort => "UnsignedShort",
                    TypeSpecifierStateMachine::UnsignedShortInt => "UnsignedShortInt",
                    TypeSpecifierStateMachine::Int => "Int",
                    TypeSpecifierStateMachine::Signed => "Signed",
                    TypeSpecifierStateMachine::SignedInt => "SignedInt",
                    TypeSpecifierStateMachine::Unsigned => "Unsigned",
                    TypeSpecifierStateMachine::UnsignedInt => "UnsignedInt",
                    TypeSpecifierStateMachine::Long => "Long",
                    TypeSpecifierStateMachine::SignedLong => "SignedLong",
                    TypeSpecifierStateMachine::LongInt => "LongInt",
                    TypeSpecifierStateMachine::SignedLongInt => "SignedLongInt",
                    TypeSpecifierStateMachine::UnsignedLong => "UnsignedLong",
                    TypeSpecifierStateMachine::UnsignedLongInt => "UnsignedLongInt",
                    TypeSpecifierStateMachine::LongLong => "LongLong",
                    TypeSpecifierStateMachine::SignedLongLong => "SignedLongLong",
                    TypeSpecifierStateMachine::LongLongInt => "LongLongInt",
                    TypeSpecifierStateMachine::SignedLongLongInt => "SignedLongLongInt",
                    TypeSpecifierStateMachine::UnsignedLongLong => "UnsignedLongLong",
                    TypeSpecifierStateMachine::UnsignedLongLongInt => "UnsignedLongLongInt",
                    TypeSpecifierStateMachine::Float => "Float",
                    TypeSpecifierStateMachine::Double => "Double",
                    TypeSpecifierStateMachine::LongDouble => "LongDouble",
                    TypeSpecifierStateMachine::Enum => "Enum",
                    TypeSpecifierStateMachine::Typedef(_) => "Typedef",
                    TypeSpecifierStateMachine::Bool => "Bool",
                    TypeSpecifierStateMachine::FloatComplex => "FloatComplex",
                    TypeSpecifierStateMachine::DoubleComplex => "DoubleComplex",
                    TypeSpecifierStateMachine::LongDoubleComplex => "LongDoubleComplex",
                    _ => unreachable!()
                }
            )),
            TyKind::Pointer(spanneds, ty_kind) => f.write_fmt(format_args!("{}*", ty_kind)),
            TyKind::Function { return_ty, parameters } => todo!(),
            TyKind::Array(ty_kind, expression) => {
                // todo: expression in the array
                f.write_fmt(format_args!("{}[]", ty_kind))
            },
        }
    }
}
impl<'hir> TyKind<'hir> {
    pub fn get_specifier_qualifier(&self) -> &SpecifierQualifiers{
        match self {
            TyKind::Base(specifier_qualifiers) => specifier_qualifiers,
            TyKind::Pointer(spanneds, ty_kind) => ty_kind.get_specifier_qualifier(),
            TyKind::Function { return_ty, parameters } => return_ty.get_specifier_qualifier(),
            TyKind::Array(ty_kind, expression) => ty_kind.get_specifier_qualifier(),
        }
    }
}