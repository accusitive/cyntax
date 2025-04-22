use cyntax_common::spanned::{Location, Spanned};
use cyntax_parser::ast::InfixOperator;
pub type HirId = usize;

#[derive(Debug)]
pub struct TranslationUnit<'hir> {
    pub declarations: Vec<&'hir ExternalDeclaration<'hir>>
}
#[derive(Debug)]
pub enum ExternalDeclaration<'hir> {
    FunctionDefinition(FunctionDefinition<'hir>),
    Declaration(&'hir Declaration<'hir>),
    X
}
#[derive(Debug)]
pub struct FunctionDefinition<'hir> {
    pub body: &'hir Statement<'hir>
}
#[derive(Debug)]
pub struct Declaration<'hir> {
    pub id: HirId,
    pub loc: Location,
    pub init: Option<&'hir Initializer<'hir>>
}
#[derive(Debug)]
pub enum Initializer<'hir>{
    Assignment(&'hir Expression<'hir>)
}
#[derive(Debug)]
pub struct Expression<'hir> {
    pub id: HirId,
    pub loc: Location,
    pub kind: ExpressionKind<'hir>
}
#[derive(Debug)]
pub enum ExpressionKind<'hir> {
    Constant(Spanned<cyntax_parser::constant::IntConstant>),
    BinaryOp(Spanned<InfixOperator>, &'hir Expression<'hir>, &'hir Expression<'hir>),
    DeclarationReference(HirId)
}
#[derive(Debug)]
pub struct Statement<'hir> {
    pub id: usize,
    pub span: Location,
    pub kind: StatementKind<'hir>
}
#[derive(Debug)]
pub enum StatementKind<'hir> {
    Compound(&'hir [BlockItem<'hir>]),
    Expression(&'hir Expression<'hir>)
}
#[derive(Debug)]
pub enum BlockItem<'hir> {
    Declaration(&'hir Declaration<'hir>),
    Statement(&'hir Statement<'hir>)
}

