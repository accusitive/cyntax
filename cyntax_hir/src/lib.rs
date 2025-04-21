use cyntax_common::spanned::{Location, Spanned};
pub type HirId = usize;

#[derive(Debug)]
pub struct TranslationUnit<'hir> {
    pub declarations: Vec<&'hir ExternalDeclaration<'hir>>
}
#[derive(Debug)]
pub enum ExternalDeclaration<'hir> {
    FunctionDefinition(FunctionDefinition<'hir>),
    Declaration(Declaration<'hir>),
    X
}
#[derive(Debug)]
pub struct FunctionDefinition<'hir> {
    body: Statement<'hir>
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
    Add(&'hir Expression<'hir>, &'hir Expression<'hir>),
    Identifier(HirId)
}
#[derive(Debug)]
pub struct Statement<'hir> {
    id: usize,
    span: Location,
    kind: StatementKind<'hir>
}
#[derive(Debug)]
pub enum StatementKind<'hir> {
    Compound(&'hir [BlockItem<'hir>])
}
#[derive(Debug)]
pub enum BlockItem<'hir> {
    Declaration,
    Statement(&'hir Statement<'hir>)
}

