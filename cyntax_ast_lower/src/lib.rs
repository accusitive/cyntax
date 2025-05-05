use std::{collections::HashMap, ops::Deref, ptr};

pub use bumpalo::Bump;
// use check::TyCheckVisitor;
use cyntax_common::{
    ctx::ParseContext,
    span,
    spanned::{Location, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_hir::{self as hir, BlockItem, FunctionParameter, HirId, HirMap, HirNode, ParsedDeclarationSpecifiers, SpecifierQualifiers, StructField, StructType, StructTypeKind, Ty, TyKind, TyQualifiers, TypeSpecifierStateMachine};
pub use cyntax_parser::ast;
use cyntax_parser::{
    ast::{DeclarationSpecifier, Identifier, IterationStatement, PrefixOperator, SpecifierQualifier},
    constant::{self, IntConstant},
};
pub mod check;
pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visit;
pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

#[derive(Debug)]
pub struct AstLower<'src, 'hir> {
    pub ctx: &'src mut ParseContext,
    pub map: HirMap<'hir>,
    arena: &'hir Bump,
    scopes: Vec<Scope>,
    next_id: usize,
}

#[derive(Debug, Default)]
pub struct Scope {
    ordinary: HashMap<ast::Identifier, HirId>,
    typedefs: HashMap<ast::Identifier, HirId>,
    tags: HashMap<ast::Identifier, HirId>,
    labels: HashMap<ast::Identifier, HirId>,
}

impl<'src, 'hir> AstLower<'src, 'hir> {
    pub fn new(ctx: &'src mut ParseContext, arena: &'hir Bump) -> Self {
        Self {
            ctx,
            arena,
            map: HirMap::new(),
            scopes: vec![],
            next_id: 0,
        }
    }
    pub fn next_id(&mut self) -> usize {
        let id = self.next_id;
        if id > 0 && !self.map.nodes.contains_key(&id) {
            // panic!("Not all ids exist in the map!")
        }
        self.next_id += 1;
        id
    }
    pub fn lower(mut self, tu: &ast::TranslationUnit) -> PResult<(&'hir hir::TranslationUnit<'hir>, Vec<cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>, HirMap<'hir>)> {
        let tu = self.lower_translation_unit(tu)?;

        // let mut tyck = TyCheckVisitor::new(&mut self);
        // tyck.visit_translation_unit(tu);

        // Ok((tu, tyck.diagnostics, self.map))
        Ok((tu, vec![], self.map))
    }
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> PResult<&'hir hir::TranslationUnit<'hir>> {
        self.push_scope();
        let mut d = vec![];
        for external_declaration in &unit.external_declarations {
            let external_declaration = self.lower_external_declaration(external_declaration)?;
            d.extend(external_declaration);
        }

        self.pop_scope();
        let tu = hir::TranslationUnit {
            declarations: self.arena.alloc_slice_fill_iter(d.into_iter()),
        };

        let tu: &'hir _ = self.arena.alloc(tu);

        Ok(tu)
    }
    pub fn lower_external_declaration(&mut self, external_declation: &ast::ExternalDeclaration) -> PResult<Vec<&'hir hir::ExternalDeclaration<'hir>>> {
        match external_declation {
            ast::ExternalDeclaration::FunctionDefinition(function_definition) => {
                let base = self.lower_declaration_ty_specifiers(&function_definition.specifiers)?;
                let ty = self.lower_ty(&base, &function_definition.declarator)?;
                if let TyKind::Function { return_ty, parameters } = &ty.kind {
                    let mut params = vec![];
                    for param in *parameters {
                        let id = self.next_id();
                        self.map.nodes.insert(id, cyntax_hir::HirNode::FunctionParameter(param));

                        if let Some(identifier) = &param.identifier {
                            self.scopes.last_mut().unwrap().ordinary.insert(identifier.value, id);
                        }
                        params.push(param);
                    }

                    let params: &'hir _ = self.arena.alloc_slice_fill_iter(params.into_iter());

                    dbg!(&return_ty, &parameters);
                    let body: &cyntax_hir::Statement<'hir> = self.lower_statement(&function_definition.body)?;
                    let base_ty = self.lower_declaration_ty_specifiers(&function_definition.specifiers)?;
                    let ty = self.lower_ty(&base_ty, &function_definition.declarator)?;

                    let id = self.next_id();
                    let fndef = self.arena.alloc(hir::FunctionDefinition {
                        id,
                        parameters: params,
                        body,
                        ty,
                        identifier: function_definition.declarator.value.get_identifier().unwrap(),
                    });
                    self.map.nodes.insert(id, HirNode::FunctionDefinition(fndef));

                    let def = hir::ExternalDeclaration::FunctionDefinition(fndef); 
                    self.scopes.last_mut().unwrap().ordinary.insert(function_definition.declarator.value.get_identifier().unwrap(), id);
                    Ok(vec![self.arena.alloc(def)])
                } else {
                    unreachable!("function definition must be of function type")
                }
            }
            ast::ExternalDeclaration::Declaration(declaration) => {
                let declarations: Vec<&'hir _> = self.lower_declaration(declaration)?;
                let external_declarations: Vec<&'hir _> = declarations.into_iter().map(|declaration| &*self.arena.alloc(hir::ExternalDeclaration::Declaration(declaration))).collect::<Vec<_>>();
                Ok(external_declarations)
            }
        }
    }
    pub fn lower_declaration(&mut self, declaration: &Spanned<ast::Declaration>) -> PResult<Vec<&'hir hir::Declaration<'hir>>> {
        let specifiers = self.lower_declaration_ty_specifiers(&declaration.value.specifiers)?;

        // let parser = DeclarationSpecifierParser::new(declaration.specifiers.iter(), &self.scopes, &mut self.map);
        // let specifiers = parser.parse()?;
        let mut d = vec![];
        for init_declarator in &declaration.value.init_declarators {
            let lowered_ty = self.lower_ty(&specifiers, &init_declarator.value.declarator)?;

            let id = self.next_id();
            let is_typedef = matches!(&specifiers.class, Some(ast::StorageClassSpecifier::Typedef));
            if let Some(identifier) = init_declarator.value.declarator.value.get_identifier() {
                if is_typedef {
                    self.scopes.last_mut().unwrap().typedefs.insert(identifier, id);
                } else {
                    self.scopes.last_mut().unwrap().ordinary.insert(identifier, id);
                }
            }
            let init = match &init_declarator.value.initializer {
                Some(init) => Some(self.lower_initializer(init)?),
                None => None,
            };
            let declaration = hir::Declaration {
                id,
                full_location: declaration.location.clone(),
                declarator_loc: init_declarator.location.clone(),
                init,
                ty: lowered_ty,
            };
            let declaration: &'hir _ = self.arena.alloc(declaration);
            self.map.nodes.insert(id, cyntax_hir::HirNode::Declaration(declaration));
            // if is_typedef {
            //     self.map.typedefs.insert(id, declaration);
            // } else {
            //     self.map.ordinary.insert(id, cyntax_hir::Ordinary::Declaration(declaration));
            // }
            d.push(declaration);
        }
        if specifiers.class.as_ref().map(|class| matches!(class, ast::StorageClassSpecifier::Typedef)).unwrap_or(false) {
            return Ok(vec![]);
        } else {
            Ok(d)
        }
    }
    // pub fn lower_declaration(&mut self, declaration: &Spanned<ast::Declaration>) -> PResult<&'hir hir::Declaration<'hir>>
    pub fn lower_initializer(&mut self, initializer: &Spanned<ast::Initializer>) -> PResult<&'hir hir::Initializer<'hir>> {
        match initializer {
            span!(ast::Initializer::Assignemnt(assignment)) => Ok(self.arena.alloc(hir::Initializer::Assignment(self.lower_expression(assignment)?))),
            span!(ast::Initializer::List(designated_intiializers)) => todo!(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    fn find_in_scope(&mut self, identifier: &Spanned<Identifier>) -> PResult<HirId> {
        // reversed, because the newest scope has priority over the 2nd newest
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.ordinary.get(&identifier.value) {
                return Ok(id);
            }
        }
        Err(SimpleError(identifier.location.clone(), format!("could not find identifier `{}` in scope", self.ctx.res(identifier.value))).into_codespan_report())
    }
    fn find_typedef_in_scope(&mut self, identifier: &Spanned<Identifier>) -> PResult<HirId> {
        // reversed, because the newest scope has priority over the 2nd newest
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.typedefs.get(&identifier.value) {
                return Ok(id);
            }
        }
        Err(SimpleError(identifier.location.clone(), format!("could not find typedef `{}` in scope", self.ctx.res(identifier.value))).into_codespan_report())
    }
    fn find_struct_in_scope(&mut self, identifier: &Spanned<Identifier>) -> PResult<HirId> {
        // reversed, because the newest scope has priority over the 2nd newest
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.tags.get(&identifier.value) {
                return Ok(id);
            }
        }
        Err(SimpleError(
            identifier.location.clone(),
            format!("could not find tag `{}` in scope, all tags: {}", self.ctx.res(identifier.value), self.all_tags(&self.tag_stack_dump())),
        )
        .into_codespan_report())
    }
    fn tag_stack_dump(&self) -> Vec<&Identifier> {
        self.scopes.iter().rev().map(|scope| scope.tags.keys()).flatten().collect()
    }
    fn all_tags(&self, tags: &[&Identifier]) -> String {
        let mut s = String::new();
        for tag in tags {
            s.push_str(self.ctx.res(**tag));
            s.push(',');
        }

        s
    }
    pub fn is_struct_type_redefinition(&mut self, tag: &Spanned<Identifier>) -> bool {
        if let Some(s) = self.scopes.last_mut().unwrap().tags.get(&tag.value) {
            let s = self.map.get_struct_ty(s).unwrap();
            if matches!(s.kind, StructTypeKind::Complete(_)) {
                return true;
            }
        }
        return false;
    }
    pub fn define_struct_type(&mut self, tag: &Spanned<Identifier>, id: HirId) -> PResult<()> {
        if let Some(s) = self.scopes.last_mut().unwrap().tags.get(&tag.value) {
            let s = self.map.get_struct_ty(s).unwrap();
            match s.kind {
                StructTypeKind::Incomplete => {
                    self.scopes.last_mut().unwrap().tags.insert(tag.value, id);
                    Ok(())
                }
                StructTypeKind::Complete(_) => Err(SimpleError(tag.location.clone(), format!("redefinition of struct type {}", self.ctx.res(tag.value))).into_codespan_report()),
            }
        } else {
            self.scopes.last_mut().unwrap().tags.insert(tag.value, id);
            Ok(())
        }
    }
}
