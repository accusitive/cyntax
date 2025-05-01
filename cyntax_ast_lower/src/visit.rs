use cyntax_hir as hir;

pub trait Visitor<'hir> {
    fn visit_translation_unit(&mut self, tu: &'hir hir::TranslationUnit<'hir>);
    fn visit_external_declaration(&mut self, ed: &'hir hir::ExternalDeclaration<'hir>);
    fn visit_function_definition(&mut self, fd: &'hir hir::FunctionDefinition<'hir>);
    fn visit_statement(&mut self, stmt: &'hir hir::Statement<'hir>);
    fn visit_expression(&mut self, expr: &'hir hir::Expression<'hir>);
    fn visit_declaration(&mut self, decl: &'hir hir::Declaration<'hir>);
}
