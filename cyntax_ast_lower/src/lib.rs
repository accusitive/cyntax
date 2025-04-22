use std::{collections::HashMap, ops::Deref};

pub use bumpalo::Bump;
use cyntax_common::{ctx::Context, span, spanned::Spanned};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_hir::{self as hir, HirId};
use cyntax_parser::ast::Identifier;
pub use cyntax_parser::ast as ast;
use ty::DeclarationSpecifierParser;

mod ty;
pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

#[derive(Debug)]
pub struct AstLower<'src, 'hir> {
    pub ctx: &'src mut Context,
    map: HirMap<'hir>,
    scopes: Vec<Scope>,
    arena: &'hir Bump,
    next_id: usize,
}
#[derive(Debug)]
pub struct HirMap<'hir> {
    // probably expression?
    ordinary: HashMap<HirId, &'hir hir::Declaration<'hir>>,
    // a type probably?
    typedefs: HashMap<HirId, ()>,
    // etc
    tags: HashMap<HirId, ()>,
    // etc, i dont even think this needs anything; labels have practically no data
    labels: HashMap<HirId, ()>,
}
#[derive(Debug, Default)]
pub struct Scope {
    ordinary: HashMap<ast::Identifier, HirId>,
    typedefs: HashMap<ast::Identifier, HirId>,
    tags: HashMap<ast::Identifier, HirId>,
    labels: HashMap<ast::Identifier, HirId>,
}

impl<'hir> HirMap<'hir> {
    pub fn new() -> Self {
        Self {
            ordinary: HashMap::new(),
            typedefs: HashMap::new(),
            tags: HashMap::new(),
            labels: HashMap::new(),
        }
    }
}

impl<'src, 'hir> AstLower<'src, 'hir> {
    pub fn new(ctx: &'src mut Context, arena: &'hir Bump) -> Self {
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
        self.next_id += 1;
        id
    }
    pub fn lower_translation_unit(&mut self, unit: &ast::TranslationUnit) -> PResult<hir::TranslationUnit<'hir>> {
        self.push_scope();
        let mut d = vec![];
        for external_declaration in &unit.external_declarations {
            let external_declaration = self.lower_external_declaration(external_declaration)?;
            d.extend(external_declaration);
        }

        self.pop_scope();
        Ok(hir::TranslationUnit { declarations: d })
    }
    pub fn lower_external_declaration(&mut self, external_declation: &ast::ExternalDeclaration) -> PResult<Vec<&'hir hir::ExternalDeclaration<'hir>>> {
        match external_declation {
            ast::ExternalDeclaration::FunctionDefinition(function_definition) => {
                let body = self.lower_statement(&function_definition.body)?;
                Ok(vec![self.arena.alloc(hir::ExternalDeclaration::FunctionDefinition(hir::FunctionDefinition { body }))])
            }
            ast::ExternalDeclaration::Declaration(declaration) => {
                // let mut parser = DeclarationSpecifierParser::new(declaration.specifiers.iter());
                // parser.parse()?;
                // dbg!(&parser);

                let mut d = vec![];
                for init_declarator in &declaration.init_declarators {
                    let id = self.next_id();
                    if let Some(identifier) = init_declarator.value.declarator.value.get_identifier() {
                        self.scopes.last_mut().unwrap().ordinary.insert(identifier, id);
                        // self.find_in_scope(&init_declarator.location.to_spanned(identifier)).unwrap();
                    }
                    let init = match &init_declarator.value.initializer {
                        Some(init) => Some(self.lower_initializer(init)?),
                        None => None,
                    };

                    let declaration = hir::Declaration { id, loc: init_declarator.location.clone(), init };
                    let declaration: &'hir _ = self.arena.alloc(declaration);
                    self.map.ordinary.insert(id, declaration);

                    let external_decl: &'hir _ = self.arena.alloc(hir::ExternalDeclaration::Declaration(declaration));
                    d.push(external_decl);
                }
                Ok(d)
            }
        }
    }
    // pub fn lower_declaration(&mut self, declaration: &Spanned<ast::Declaration>) -> PResult<&'hir hir::Declaration<'hir>>
    pub fn lower_initializer(&mut self, initializer: &Spanned<ast::Initializer>) -> PResult<&'hir hir::Initializer<'hir>> {
        match initializer {
            span!(ast::Initializer::Assignemnt(assignment)) => Ok(self.arena.alloc(hir::Initializer::Assignment(self.lower_expression(assignment)?))),
            span!(ast::Initializer::List(designated_intiializers)) => todo!(),
        }
    }
    pub fn lower_statement(&mut self, statement: &Spanned<ast::Statement>) -> PResult<&'hir hir::Statement<'hir>> {
        let kind = match &statement.value {
            ast::Statement::Labeled(labeled_statement) => todo!(),
            ast::Statement::Compound(block_items) => {
                self.push_scope();
                let mut hir_block_items = vec![];
                for item in block_items {
                    match item {
                        ast::BlockItem::Declaration(declaration) => {
                            let parser = DeclarationSpecifierParser::new(declaration.value.specifiers.iter());
                            let parsed = parser.parse()?;
                            dbg!(&parsed);
                            for init_declarator in &declaration.value.init_declarators {
                                // Declare it before lowering initializer, otherwise `int a = 0 + a;` doesnt work
                                let id = self.next_id();
                                if let Some(identifier) = init_declarator.value.declarator.value.get_identifier() {
                                    self.scopes.last_mut().unwrap().ordinary.insert(identifier, id);
                                }

                                let init = match &init_declarator.value.initializer {
                                    Some(init) => Some(self.lower_initializer(init)?),
                                    None => None,
                                };

                                let declaration = hir::Declaration { id, loc: init_declarator.location.clone(), init };
                                let declaration: &'hir _ = self.arena.alloc(declaration);
                                self.map.ordinary.insert(id, declaration);

                                hir_block_items.push(hir::BlockItem::Declaration(declaration));
                            }
                        }
                        ast::BlockItem::Statement(statement) => {
                            hir_block_items.push(hir::BlockItem::Statement(self.lower_statement(&statement)?));
                        }
                    }
                }
                self.pop_scope();
                hir::StatementKind::Compound(self.arena.alloc_slice_fill_iter(hir_block_items.into_iter()))
            }
            ast::Statement::Expression(expression) => hir::StatementKind::Expression(self.arena.alloc(self.lower_expression(expression)?)),
            ast::Statement::Iteration(iteration_statement) => todo!(),
            ast::Statement::Goto(spanned) => todo!(),
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
            ast::Statement::Error => todo!(),
            ast::Statement::Return(spanned) => todo!(),
            ast::Statement::If(spanned, statement, statement1) => todo!(),
            ast::Statement::Switch(spanned, statement) => todo!(),
        };

        let id = self.next_id();
        Ok(self.arena.alloc(hir::Statement { id, span: statement.location.clone(), kind }))
    }
    pub fn lower_expression(&mut self, expression: &Spanned<ast::Expression>) -> PResult<&'hir hir::Expression<'hir>> {
        let kind = match &expression.value {
            ast::Expression::Identifier(identifier) => {
                let hir_id = self.find_in_scope(identifier)?;
                hir::ExpressionKind::DeclarationReference(hir_id)
            }
            ast::Expression::IntConstant(constant) => hir::ExpressionKind::Constant(constant.clone()),
            ast::Expression::StringLiteral(literal) => todo!(),
            ast::Expression::Parenthesized(expr) => todo!(),
            ast::Expression::BinOp(op, lhs, rhs) => {
                let lhs = self.lower_expression(lhs.deref())?;
                let rhs = self.lower_expression(rhs.deref())?;
                hir::ExpressionKind::BinaryOp(op.clone(), lhs, rhs)
            }
            ast::Expression::UnaryOp(op, expr) => todo!(),
            ast::Expression::PostfixOp(op, expr) => todo!(),
            ast::Expression::Cast(type_name, expr) => todo!(),
            ast::Expression::Call(expr, args) => todo!(),
            ast::Expression::Subscript(expr, offset) => todo!(),
            ast::Expression::Ternary(control, then, elze) => todo!(),
            ast::Expression::Sizeof(type_name) => todo!(),
            ast::Expression::Null => todo!(),
        };
        let id = self.next_id();
        Ok(self.arena.alloc(hir::Expression { id, kind, loc: expression.location.clone() }))
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
}
