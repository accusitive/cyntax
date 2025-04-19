use cyntax_common::{
    ast::{Keyword, Punctuator},
    ctx::string_interner::symbol::SymbolU32,
    spanned::Spanned,
};

use crate::constant::IntConstant;
pub type Identifier = SymbolU32;

#[derive(Debug)]
pub struct TranslationUnit {
    pub external_declarations: Vec<ExternalDeclaration>,
}
#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}
#[derive(Debug)]
pub struct FunctionDefinition {
    pub specifiers: Vec<Spanned<DeclarationSpecifier>>,
    pub declarator: Spanned<Declarator>,
    pub body: Statement, // pub declaration_list: Vec<Declaration>,
}
#[derive(Debug)]
pub enum DeclarationSpecifier {
    StorageClass(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier,
}
#[derive(Debug)]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}
#[derive(Debug)]
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
    /// _Bool
    Bool,
    /// _Complex
    Complex,
    Struct(StructSpecifier),
    Enum,
    TypedefName(Identifier),
}
#[derive(Debug)]
pub struct StructSpecifier {
    pub identifier: Option<Identifier>,
    pub declarations: Vec<StructDeclaration>,
}
#[derive(Debug)]
pub struct StructDeclaration {
    pub specifier_qualifiers: Vec<SpecifierQualifier>,
    pub declarators: Vec<StructDeclarator>,
}
#[derive(Debug)]
pub struct StructDeclarator {
    pub declarator: Option<Spanned<Declarator>>,
    // bitfield: Expression
}
#[derive(Debug)]
pub enum SpecifierQualifier {
    Specifier(TypeSpecifier),
    Qualifier(TypeQualifier),
}
#[derive(Debug)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}
impl From<Keyword> for TypeQualifier {
    fn from(value: Keyword) -> Self {
        match value {
            Keyword::Const => Self::Const,
            Keyword::Restrict => Self::Restrict,
            Keyword::Volatile => Self::Volatile,
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub enum Declarator {
    Identifier(Identifier),
    /// *declarator
    Pointer(Spanned<Pointer>, Box<Spanned<Self>>),
    /// (declarator)
    Parenthesized(Box<Spanned<Self>>),
    // todo: array
    Function(Box<Spanned<Self>>, Vec<Spanned<ParameterDeclaration>>),
    Array {
        base: Box<Spanned<Self>>,
        has_static: bool,
        has_star: bool,

        type_qualifiers: Vec<Spanned<TypeQualifier>>,
        expr: Option<Box<Spanned<Expression>>>,
    },
    // maybe this shouldnt be in the main declarator?
    Abstract,
}
#[derive(Debug)]
pub struct Pointer {
    pub type_qualifiers: Vec<Spanned<TypeQualifier>>,
    pub ptr: Option<Box<Spanned<Self>>>,
}
#[derive(Debug)]
pub struct ParameterDeclaration {
    pub specifiers: Vec<Spanned<DeclarationSpecifier>>,
    pub declarator: Option<Spanned<Declarator>>,
}
#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Spanned<Declarator>,
    pub initializer: Option<Spanned<Initializer>>,
}
#[derive(Debug)]
pub struct Declaration {
    pub specifiers: Vec<Spanned<DeclarationSpecifier>>,
    pub init_declarators: Vec<Spanned<InitDeclarator>>,
}
#[derive(Debug)]
pub struct ParameterList {
    pub parameters: Vec<ParameterDeclaration>,
    pub variadic: bool,
}
#[derive(Debug)]
pub enum Statement {
    Labeled(LabeledStatement),
    Compound(Vec<BlockItem>),
    Expression(Spanned<Expression>),
    Iteration(IterationStatement),
    Goto(Spanned<Identifier>),
    Continue,
    Break,
    Error,
    Return(Option<Spanned<Expression>>),
    If(Spanned<Expression>, Box<Statement>, Option<Box<Statement>>),
    Switch(Spanned<Expression>, Box<Statement>),
}
#[derive(Debug)]
pub enum LabeledStatement {
    Identifier(Spanned<Identifier>, Box<Statement>),
    Case(Spanned<Expression>, Box<Statement>),
    Default(Box<Statement>),
}
#[derive(Debug)]
pub enum IterationStatement {
    While(Spanned<Expression>, Box<Statement>),
    DoWhile(Box<Statement>, Spanned<Expression>),
    ForLoop {
        init: Option<ForInit>,
        condition: Option<Spanned<Expression>>,
        update: Option<Spanned<Expression>>,
        body: Box<Statement>,
    },
}
#[derive(Debug)]
pub enum ForInit {
    Expression(Spanned<Expression>),
    Declaration(Spanned<Declaration>),
}
#[derive(Debug)]
pub enum BlockItem {
    Declaration(Spanned<Declaration>),
    Statement(Statement),
}
#[derive(Debug)]
pub enum Initializer {
    Assignemnt(Spanned<Expression>),
    List(Vec<DesignatedIntiializer>),
}
#[derive(Debug)]
pub struct DesignatedIntiializer {
    pub designation: Vec<Designator>,
    pub initializer: Spanned<Initializer>,
}
#[derive(Debug)]
pub enum Designator {
    // [constant-expression]
    ConstantExpression,
    Identifier(Spanned<Identifier>),
}
#[derive(Debug)]
pub enum Expression {
    Identifier(Spanned<Identifier>),
    IntConstant(Spanned<IntConstant>),
    StringLiteral(Spanned<SymbolU32>),
    Parenthesized(Box<Spanned<Self>>),
    BinOp(Spanned<InfixOperator>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    UnaryOp(Spanned<PrefixOperator>, Box<Spanned<Self>>),
    PostfixOp(Spanned<PostfixOperator>, Box<Spanned<Self>>),
    Cast(Spanned<TypeName>, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    Subscript(Box<Spanned<Self>>, Box<Spanned<Self>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    StringLiteral(SymbolU32),
    CharLiteral(SymbolU32),
    Punctuator(Punctuator),
    Constant(IntConstant),
}
#[derive(Debug)]
pub enum Operator {
    Prefix(PrefixOperator),
    Infix(InfixOperator),
    Postfix(PostfixOperator),
}
#[derive(Debug)]
pub enum PrefixOperator {
    Plus,
    Minus,

    LogicalNot,

    /// ~
    BitwiseNot,

    SizeOf,
    CastOrParen,
    Dereference,
    Increment,
    Decrement,
}
#[derive(Debug)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,

    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    BitwiseShiftRightAssign,
    BitwiseShiftLeftAssign,
    // a.b
    Access,
    // a->b
    IndirectAccess,
}
#[derive(Debug)]
pub enum PostfixOperator {
    Increment,
    Decrement,
    Call,
    Subscript,
}

#[derive(Debug)]
pub struct TypeName {
    pub specifier_qualifiers: Vec<SpecifierQualifier>,
    pub declarator: Spanned<Declarator>,
}
