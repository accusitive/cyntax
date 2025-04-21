use std::collections::HashMap;

use bumpalo::Bump;
/// overrall plans:
/// parse type specifiers
/// int a, b; -> int a; int b;
/// erase typedefs
/// give names to anonymous struct/union/enums
/// reject A LOT of invalid programs
use cyntax_common::{ctx::Context, span, spanned::Spanned};
use cyntax_errors::{ errors::SimpleError, Diagnostic};
use cyntax_hir as hir;
use cyntax_parser::ast::{self, BlockItem, Identifier, LabeledStatement, StorageClassSpecifier, TypeSpecifier};
use typed_arena::Arena;

pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

pub type DeclarationId = usize;
#[derive(Debug)]
pub enum HirNode {

}
#[derive(Debug)]
pub struct Scope {
    labels: HashMap<ast::Identifier, usize>,
}
#[derive(Debug)]
pub struct AstLower<'src> {
    ctx: &'src mut Context,
    resolver: Resolver,
    scopes: Vec<Scope>,
    arena: Bump,
    next_id: usize
}
#[derive(Debug)]
pub struct Resolver {
    ordinary: Vec<Identifier>,
    typedefs: Vec<Identifier>,
    tags: Vec<Identifier>,
    labels: Vec<Identifier>,
    // HashMap<DeclarationId, Vec<Identifier>> for member name space??
}
impl Resolver {
    pub fn new() -> Self {
        Self {
            ordinary: vec![],
            typedefs: vec![],
            tags: vec![],
            labels: vec![],
        }
    }
    pub fn declare_function(&mut self, d: &ast::Declarator) {
        if let Some(name) = d.get_identifier() {
            self.ordinary.push(name);
        }
    }
    pub fn declare_label(&mut self, label: Identifier) -> usize {
        let id = self.labels.len();
        self.labels.push(label);
        id
    }
    
}

impl<'src, 'hir> AstLower<'src> {
    pub fn new(ctx: &'src mut Context) -> Self {
        Self {
            ctx,
            resolver: Resolver::new(),
            scopes: vec![Scope { labels: HashMap::new() }],
            arena: Bump::new(),
            next_id: 0
        }
    }
    pub fn next_id(&mut self) -> usize {
        let id = self.next_id;

        self.next_id += 1;
        id
    }
    // fn next_declaration_id(&self) -> DeclarationId {

    // }
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> PResult<hir::TranslationUnit> {

        todo!();
        // let mut decls = vec![];
        // for external_declaration in &unit.external_declarations {
            // decls.extend(self.lower_external_declaration(external_declaration)?);
        // }
        // let decls = unit.external_declarations.iter().map(|decl| self.lower_external_declaration(decl)).collect::<PResult<Vec<_>>>()?.into_iter().flatten().collect();
        // Ok(hir::TranslationUnit { declarations: decls })
    }
    // pub fn lower_external_declaration(&mut self, declaration: &ast::ExternalDeclaration) -> PResult<Vec<hir::ExternalDeclaration>> {
    //     match declaration {
    //         ast::ExternalDeclaration::FunctionDefinition(function_definition) => {
    //             dbg!(&function_definition);
    //             self.lower_function_definition(function_definition)?;

    //             // Ok(vec![self.lower_function_definition(function_definition)])
    //             // todo!()
    //             return Ok(vec![])
    //         }
    //         ast::ExternalDeclaration::Declaration(declaration) => {
    //             let mut declarations = vec![];
    //             // if declaration.specifiers.iter().any(|spec| matches!(spec, span!(DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef)))) {
    //             // } else {
    //             for init_declarator in &declaration.init_declarators {

    //                 // self.resolver.declare_function(init);
    //                 // self.declarations.push(init_declarator.value.declarator.value.get_identifier().unwrap());
    //                 // declarations.push(hir::ExternalDeclaration::Declaration(hir::Declaration { type_specifiers: vec![] }));
    //             }
    //             // }

    //             Ok(declarations)
    //         }
    //     }
    // }
    // pub fn lower_function_definition(&mut self, function_definition: &ast::FunctionDefinition) -> PResult<()>{
    //     self.resolver.declare_function(&function_definition.declarator.value);

    //     self.lower_statement(&function_definition.body)?;
    //     Ok(())
    // }
    // pub fn lower_statement(&mut self, stmt: &ast::Statement) -> PResult<()>{
    //     match stmt {
    //         ast::Statement::Labeled(labeled_statement) => {
    //             if let LabeledStatement::Identifier(identifier, body) = labeled_statement {
    //                 let label_id = self.resolver.declare_label(identifier.value);
    //                 self.scopes.last_mut().unwrap().labels.insert(identifier.value, label_id);
    //             }
    //         }
    //         ast::Statement::Compound(block_items) => {
    //             self.push_scope();
    //             for item in block_items {
    //                 if let BlockItem::Statement(stmt) = item {
    //                     self.lower_statement(stmt)?;
    //                 }
    //             }
    //             self.pop_scope();
    //         }
    //         ast::Statement::Expression(spanned) => todo!(),
    //         ast::Statement::Iteration(iteration_statement) => todo!(),
    //         ast::Statement::Goto(spanned) => {
    //             let x = self.resolve_label(&spanned)?;
    //             dbg!(&x);
    //         }
    //         ast::Statement::Continue => todo!(),
    //         ast::Statement::Break => todo!(),
    //         ast::Statement::Error => todo!(),
    //         ast::Statement::Return(spanned) => todo!(),
    //         ast::Statement::If(spanned, statement, statement1) => todo!(),
    //         ast::Statement::Switch(spanned, statement) => todo!(),
    //     }
    //     Ok(())
    // }
    fn push_scope(&mut self) {
        // self.scopes.push(Scope::new());
    }
    fn pop_scope(&mut self) {}
    fn resolve_label(&self, label: &Spanned<Identifier>) -> PResult<usize> {
        for scope in &self.scopes {
            if let Some(label_id) = scope.labels.get(&label.value) {
                return Ok(*label_id);
            }
        }
        Err(SimpleError(label.location.clone(), format!("Could not resolve label `{}`", self.ctx.res(label.value))).into_codespan_report())
    }
}
