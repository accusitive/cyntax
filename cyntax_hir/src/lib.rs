use cyntax_common::spanned::{Location, Spanned};

#[derive(Debug)]
pub struct TranslationUnit {
    pub declarations: Vec<ExternalDeclaration>
}
#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition,
    Declaration(Declaration)

}
#[derive(Debug)]
pub struct FunctionDefinition<'hir> {
    body: Statement<'hir>
}
#[derive(Debug)]
pub struct Declaration {
    pub type_specifiers: Vec<Spanned<cyntax_parser::ast::DeclarationSpecifier>>

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

