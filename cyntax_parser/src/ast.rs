use cyntax_common::{ast::Keyword, spanned::Spanned};

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
    TypedefName(String),
}
#[derive(Debug)]
pub struct StructSpecifier {
    pub identifier: Option<String>,
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
            _ => panic!()
        }
    }
}

#[derive(Debug)]
pub enum Declarator {
    Identifier(String),
    /// *declarator
    Pointer(Spanned<Pointer>, Box<Spanned<Self>>),
    /// (declarator)
    Parenthesized(Box<Spanned<Self>>),
    // todo: array
    Function(Box<Spanned<Self>>, Vec<Spanned<ParameterDeclaration>>),
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
    pub declarator: Spanned<Declarator>,
}

#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Spanned<Declarator>,
    pub initializer: Option<Initializer>,
}
#[derive(Debug)]
pub struct Declaration {
    pub specifiers: Vec<Spanned<DeclarationSpecifier>>,
    pub init_declarators: Vec<InitDeclarator>,
}
#[derive(Debug)]
pub struct ParameterList {
    pub parameters: Vec<ParameterDeclaration>,
    pub variadic: bool,
}
#[derive(Debug)]
pub enum Statement {
    Labeled,
    Compound(Vec<BlockItem>),
    Expression,
    Selection,
    Iteration,
    Goto(Spanned<String>),
    Continue,
    Break,
    Error,
    Return(Option<Expression>),
}
#[derive(Debug)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}
#[derive(Debug)]
pub enum Initializer {
    Assignemnt,
    List(Vec<DesignatedIntiializer>),
}
#[derive(Debug)]
pub struct DesignatedIntiializer {
    pub designation: Vec<Designator>,
    pub initializer: Initializer,
}
#[derive(Debug)]
pub enum Designator {
    // [constant-expression]
    ConstantExpression,
    Identifier(Spanned<String>),
}
#[derive(Debug)]
pub enum Expression {
    Identifier(String)
}