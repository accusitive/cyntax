use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::Diagnostic;
use cyntax_parser::{PResult, ast};
pub type HirId = usize;

#[derive(Debug)]
pub struct TranslationUnit<'hir> {
    pub declarations: Vec<&'hir ExternalDeclaration<'hir>>,
}
#[derive(Debug)]
pub enum ExternalDeclaration<'hir> {
    FunctionDefinition(FunctionDefinition<'hir>),
    Declaration(&'hir Declaration<'hir>),
    X,
}
#[derive(Debug)]
pub struct FunctionDefinition<'hir> {
    pub body: &'hir Statement<'hir>,
}
#[derive(Debug)]
pub struct Declaration<'hir> {
    pub id: HirId,
    pub loc: Location,
    pub init: Option<&'hir Initializer<'hir>>,
    pub ty: &'hir Ty<'hir>,
}
#[derive(Debug)]
pub struct StructType {
    pub id: HirId,
    pub kind: StructTypeKind
}
#[derive(Debug)]
pub enum StructTypeKind {
    Incomplete
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
    pub kind: TyKind<'hir>
}
#[derive(Debug, Clone)]
pub enum TyKind<'hir> {
    Base(SpecifierQualifiers),
    Pointer(Vec<Spanned<ast::TypeQualifier>>, Box<TyKind<'hir>>),
    Function { return_ty: Box<TyKind<'hir>>, parameters: &'hir [&'hir Ty<'hir>] },
    Array(Box<Self>, &'hir Expression<'hir>)
}

#[derive(Debug, Clone)]
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
