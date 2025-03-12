use crate::preprocess::ast::Constant;

use super::L;

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub declarations: Vec<L<ExternalDeclaration>>,
}
#[derive(Debug, Clone)]
pub enum ExternalDeclaration {
    Declaration(L<Declaration>),
    FunctionDefinition(L<Declaration>, Statement),
}
#[derive(Debug, Clone)]
pub struct Declaration {
    pub specifiers: Vec<L<DeclarationSpecifier>>,
    pub init_delcarators: Vec<L<InitDeclarator>>,
}
#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub declarator: L<Declarator>,
    pub init: Option<L<Expression>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeclaration {
    pub specifiers: Vec<L<DeclarationSpecifier>>,
    pub declarator: L<Declarator>,
    pub bitfield: Option<L<Expression>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationSpecifier {
    StorageClass(StorageClass),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    /// _bool
    Bool,
    /// _complex
    Complex,
    /// typedef-name
    TypedefName(String),
    Struct(StructSpecifier),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructSpecifier {
    pub name: Option<L<String>>,
    pub declarations: StructCompleteness,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructCompleteness {
    Complete(Vec<StructDeclaration>),
    Incomplete,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StorageClass {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declarator {
    // identifier: D<String>,
    pub kind: DeclaratorKind,
    pub derived: Vec<DerivedDeclarator>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeName {
    pub specifiers: Vec<L<DeclarationSpecifier>>,
    /// Always abstract
    pub declarator: L<Declarator>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclaratorKind {
    Identifier(L<String>),
    Declarator(Box<L<Declarator>>),
    Abstract,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DerivedDeclarator {
    Array(L<Expression>),
    /// parameters, is_variadic
    Function(Vec<Parameter>, bool),
    /// Technically everything within Pointer should be type qualifier, but also parsing specifiers is fine since we can deny it later
    Pointer(Vec<L<DeclarationSpecifier>>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub specifiers: Vec<L<DeclarationSpecifier>>,
    pub declarator: L<Declarator>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
    Call(String, Vec<L<Self>>),
    Constant(Constant),
    BinOp(Box<L<Self>>, L<BinaryOperation>, Box<L<Self>>),
    // PostfixOp(Box<L<Self>>, PostfixOperator),
    UnaryOp(Box<L<Self>>, L<UnaryOperator>),
    Member(Box<L<Self>>, L<String>),
    StringLiteral(String),
    Conditional(ConditionalExpression),
    Comma(Vec<L<Self>>),
    Cast(L<TypeName>, Box<L<Self>>),
    Defined(String),
    SizeOf(L<TypeName>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    /// x++
    Increment,
    /// !x
    Negate,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalExpression {
    pub condition: Box<L<Expression>>,
    pub r#true: Box<Option<L<Expression>>>,
    pub r#false: Box<Option<L<Expression>>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    /// %
    Mod,
    Less,
    Greater,
    LessOrEqual,
    GreatorOrEqual,
    /// =
    Assign,
    /// ==
    Compare,
    /// +=
    AssignBySum,
    /// -=
    AssignByDifference,
    /// *=
    AssignByProduct,
    /// /=
    AssignByQuotient,
    /// %=
    AssignByRemainder,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    /// x ^ y
    BitwiseXor,
    BitwiseOr,
}
#[derive(Debug, Clone)]
pub enum Statement {
    Expression(L<Expression>),
    Block(Vec<Self>),
    Declaration(L<Declaration>),
    Return(L<Expression>),
    If(L<Expression>, Box<Self>),
    IfElse(L<Expression>, Box<Self>, Box<Self>),
    For(Option<ForInitializor>, Option<L<Expression>>, Option<L<Expression>>, Box<Self>),
}
#[derive(Debug, Clone)]

pub enum ForInitializor {
    Declaration(L<Declaration>),
    Expression(L<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum Keyword {
    Auto, Break,  
    Case, Char, 
    Const, Continue,
    Default, Do,   
    Double, Else,  
    Enum, Extern,  
    Float, For,  
    Goto, If,   
    Inline, Int,
    Long, Register, 
    Restrict, Return,
    Short, Signed,  
    Sizeof, Static, 
    Struct, Switch,  
    Typedef, Union, 
    Unsigned, Void, 
    Volatile, While, 
    Bool, Complex,  
    Imaginary,
}

impl Keyword {
    #[rustfmt::skip]
    pub fn from_str(identifier: &str) -> Option<Self> {
        match identifier {
            "auto" => Some(Keyword::Auto), "break" => Some(Keyword::Break),
            "case" => Some(Keyword::Case), "char" => Some(Keyword::Char),
            "const" => Some(Keyword::Const), "continue" => Some(Keyword::Continue),
            "default" => Some(Keyword::Default), "do" => Some(Keyword::Do),
            "double" => Some(Keyword::Double), "else" => Some(Keyword::Else),
            "enum" => Some(Keyword::Enum), "extern" => Some(Keyword::Extern),
            "float" => Some(Keyword::Float), "for" => Some(Keyword::For),
            "goto" => Some(Keyword::Goto), "if" => Some(Keyword::If),
            "inline" => Some(Keyword::Inline), "int" => Some(Keyword::Int),
            "long" => Some(Keyword::Long), "register" => Some(Keyword::Register),
            "restrict" => Some(Keyword::Restrict), "return" => Some(Keyword::Return),
            "short" => Some(Keyword::Short), "signed" => Some(Keyword::Signed),
            "sizeof" => Some(Keyword::Sizeof), "static" => Some(Keyword::Static),
            "struct" => Some(Keyword::Struct), "switch" => Some(Keyword::Switch),
            "typedef" => Some(Keyword::Typedef), "union" => Some(Keyword::Union),
            "unsigned" => Some(Keyword::Unsigned), "void" => Some(Keyword::Void),
            "volatile" => Some(Keyword::Volatile), "while" => Some(Keyword::While),
            "_Bool" => Some(Keyword::Bool), "_Complex" => Some(Keyword::Complex),
            "_Imaginary" => Some(Keyword::Imaginary),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::Auto => "auto",
            Keyword::Break => "break",
            Keyword::Case => "case",
            Keyword::Char => "char",
            Keyword::Const => "const",
            Keyword::Continue => "continue",
            Keyword::Default => "default",
            Keyword::Do => "do",
            Keyword::Double => "double",
            Keyword::Else => "else",
            Keyword::Enum => "enum",
            Keyword::Extern => "extern",
            Keyword::Float => "float",
            Keyword::For => "for",
            Keyword::Goto => "goto",
            Keyword::If => "if",
            Keyword::Inline => "inline",
            Keyword::Int => "int",
            Keyword::Long => "long",
            Keyword::Register => "register",
            Keyword::Restrict => "restrict",
            Keyword::Return => "return",
            Keyword::Short => "short",
            Keyword::Signed => "signed",
            Keyword::Sizeof => "sizeof",
            Keyword::Static => "static",
            Keyword::Struct => "struct",
            Keyword::Switch => "switch",
            Keyword::Typedef => "typedef",
            Keyword::Union => "union",
            Keyword::Unsigned => "unsigned",
            Keyword::Void => "void",
            Keyword::Volatile => "volatile",
            Keyword::While => "while",
            Keyword::Bool => "_Bool",
            Keyword::Complex => "_Complex",
            Keyword::Imaginary => "_Imaginary",
        }
    }
}
