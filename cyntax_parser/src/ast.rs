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
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
    // pub declaration_list: Vec<Declaration>,
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
    Register
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
    Struct,
    Enum,
    TypedefName(String)
}
#[derive(Debug)]
pub enum TypeQualifier {
    Const, Restrict, Volatile
}
#[derive(Debug)]
pub enum Declarator {
    Identifier(String),
    /// *declarator
    Pointer(Pointer, Box<Self>),
    /// (declarator)
    Parenthesized(Box<Self>),
    // todo: array
    Function(Box<Self>, Vec<ParameterDeclaration>),
    // maybe this shouldnt be in the main declarator?
    Abstract
}
#[derive(Debug)]
pub struct Pointer {
    pub type_qualifiers: Vec<TypeQualifier>,
    pub ptr: Option<Box<Self>>
}
#[derive(Debug)]
pub struct ParameterDeclaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
}

#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Declarator
}
#[derive(Debug)]
pub struct Declaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub init_declarators: Vec<InitDeclarator>

}
#[derive(Debug)]
pub struct ParameterList {
    pub parameters: Vec<ParameterDeclaration>,
    pub variadic: bool
}