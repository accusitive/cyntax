use std::{collections::HashMap, ops::Deref, ptr};

pub use bumpalo::Bump;
use check::TyCheckVisitor;
use cyntax_common::{
    ctx::ParseContext,
    span,
    spanned::{Location, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_hir::{self as hir, BlockItem, FunctionParameter, HirId, HirMap, ParsedDeclarationSpecifiers, SpecifierQualifiers, StructField, StructType, StructTypeKind, Ty, TyKind, TyQualifiers, TypeSpecifierStateMachine};
pub use cyntax_parser::ast;
use cyntax_parser::{
    ast::{DeclarationSpecifier, Identifier, IterationStatement, PrefixOperator, SpecifierQualifier},
    constant::{self, IntConstant},
};
use visit::Visitor;
pub mod check;
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
                    for param in *parameters {
                        let id = self.next_id();
                        let declaration = hir::Declaration {
                            id,
                            declarator_loc: Location::new(),
                            full_location: Location::new(),
                            init: None,
                            ty,
                        };
                        let declaration: &'hir _ = self.arena.alloc(declaration);
                        self.map.ordinary.insert(id, declaration);

                        if let Some(identifier) = &param.identifier {
                            self.scopes.last_mut().unwrap().ordinary.insert(identifier.value, id);
                        }
                    }

                    dbg!(&return_ty, &parameters);
                    // self.scopes.last_mut().unwrap().ordinary.
                }
                let body: &cyntax_hir::Statement<'hir> = self.lower_statement(&function_definition.body)?;
                let base_ty = self.lower_declaration_ty_specifiers(&function_definition.specifiers)?;
                let ty = self.lower_ty(&base_ty, &function_definition.declarator)?;

                let def = hir::ExternalDeclaration::FunctionDefinition(hir::FunctionDefinition { body , ty, identifier: function_definition.declarator.value.get_identifier().unwrap()});

                Ok(vec![self.arena.alloc(def)])
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
            if is_typedef {
                self.map.typedefs.insert(id, declaration);
            } else {
                self.map.ordinary.insert(id, declaration);
            }
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
    fn lower_declaration_ty_specifiers(&mut self, specifiers: &[Spanned<DeclarationSpecifier>]) -> PResult<ParsedDeclarationSpecifiers> {
        let mut base_type = TypeSpecifierStateMachine::None;
        let mut qualifier = TyQualifiers { conzt: false, restrict: false, volatile: false };
        let mut class = None;

        let mut last_location = Location::new();

        for specifier in specifiers {
            let loc = specifier.location.clone();
            last_location = loc.clone();
            match &specifier.value {
                ast::DeclarationSpecifier::StorageClass(storage_class_specifier) => match storage_class_specifier {
                    _ if class.is_some() => return Err(SimpleError(specifier.location.clone(), format!("already have a storage class")).into_codespan_report()),
                    storage_class => class = Some(storage_class),
                },
                ast::DeclarationSpecifier::TypeSpecifier(type_specifier) => match type_specifier {
                    ast::TypeSpecifier::Void => base_type = base_type.void(loc)?,
                    ast::TypeSpecifier::Char => base_type = base_type.char(loc)?,
                    ast::TypeSpecifier::Short => base_type = base_type.short(loc)?,
                    ast::TypeSpecifier::Int => base_type = base_type.int(loc)?,
                    ast::TypeSpecifier::Long => base_type = base_type.long(loc)?,
                    ast::TypeSpecifier::Float => base_type = base_type.float(loc)?,
                    ast::TypeSpecifier::Double => base_type = base_type.double(loc)?,
                    ast::TypeSpecifier::Signed => base_type = base_type.signed(loc)?,
                    ast::TypeSpecifier::Unsigned => base_type = base_type.unsigned(loc)?,
                    ast::TypeSpecifier::Bool => base_type = base_type.bool(loc)?,
                    ast::TypeSpecifier::TypedefName(typedef_name) => {
                        let t = self.find_typedef_in_scope(&loc.to_spanned(*typedef_name))?;

                        base_type = base_type.typedef_name(loc, t)?;
                    }
                    ast::TypeSpecifier::Struct(specifier) => {
                        let id = match &specifier.tag {
                            Some(tag) => {
                                if let Ok(s) = self.find_struct_in_scope(tag) {
                                    s
                                } else {
                                    self.lower_struct_ty_specifier(specifier)?
                                }
                            }
                            // Some(tag) if specifier.declarations.is_none() => self.find_struct_in_scope(tag)?,
                            _ => self.lower_struct_ty_specifier(specifier)?,
                        };

                        // let id =
                        base_type = base_type.struct_or_union(loc, id)?;
                    }
                    x => unimplemented!("{x:?}"),
                },
                ast::DeclarationSpecifier::TypeQualifier(type_qualifier) => match type_qualifier {
                    ast::TypeQualifier::Const => qualifier.conzt = true,
                    ast::TypeQualifier::Restrict => qualifier.restrict = true,
                    ast::TypeQualifier::Volatile => qualifier.volatile = true,
                },
                ast::DeclarationSpecifier::FunctionSpecifier(function_specifier) => todo!(),
            }
        }
        if let TypeSpecifierStateMachine::None = base_type {
            return Err(SimpleError(last_location, format!("must have at least 1 type specifier")).into_codespan_report());
        }

        Ok(ParsedDeclarationSpecifiers {
            class: class.cloned(),
            specifiers: base_type,
            qualifier: qualifier,
        })
    }
    fn lower_ty_specifiers_qualifiers(&mut self, specifiers: &[Spanned<SpecifierQualifier>]) -> PResult<SpecifierQualifiers> {
        let mut base_type = TypeSpecifierStateMachine::None;
        let mut qualifier = TyQualifiers { conzt: false, restrict: false, volatile: false };

        let mut last_location = Location::new();

        for specifier in specifiers {
            let loc = specifier.location.clone();
            last_location = loc.clone();
            match &specifier.value {
                ast::SpecifierQualifier::Specifier(type_specifier) => match type_specifier {
                    ast::TypeSpecifier::Void => base_type = base_type.void(loc)?,
                    ast::TypeSpecifier::Char => base_type = base_type.char(loc)?,
                    ast::TypeSpecifier::Short => base_type = base_type.short(loc)?,
                    ast::TypeSpecifier::Int => base_type = base_type.int(loc)?,
                    ast::TypeSpecifier::Long => base_type = base_type.long(loc)?,
                    ast::TypeSpecifier::Float => base_type = base_type.float(loc)?,
                    ast::TypeSpecifier::Double => base_type = base_type.double(loc)?,
                    ast::TypeSpecifier::Signed => base_type = base_type.signed(loc)?,
                    ast::TypeSpecifier::Unsigned => base_type = base_type.unsigned(loc)?,
                    ast::TypeSpecifier::Bool => base_type = base_type.bool(loc)?,
                    ast::TypeSpecifier::TypedefName(typedef_name) => {
                        let t = self.find_typedef_in_scope(&loc.to_spanned(*typedef_name))?;

                        base_type = base_type.typedef_name(loc, t)?;
                    }
                    ast::TypeSpecifier::Struct(specifier) => {
                        let id = self.lower_struct_ty_specifier(specifier)?;
                        base_type = base_type.struct_or_union(loc, id)?;
                    }
                    x => unimplemented!("{x:?}"),
                },
                ast::SpecifierQualifier::Qualifier(type_qualifier) => match type_qualifier {
                    ast::TypeQualifier::Const => qualifier.conzt = true,
                    ast::TypeQualifier::Restrict => qualifier.restrict = true,
                    ast::TypeQualifier::Volatile => qualifier.volatile = true,
                },
            }
        }
        if let TypeSpecifierStateMachine::None = base_type {
            return Err(SimpleError(last_location, format!("must have at least 1 type specifier")).into_codespan_report());
        }

        Ok(SpecifierQualifiers { specifiers: base_type, qualifier })
    }
    fn lower_struct_ty_specifier(&mut self, specifier: &ast::StructOrUnionSpecifier) -> PResult<HirId> {
        let id = self.next_id();
        let struct_ty = StructType {
            id,
            tag: specifier.tag.clone(),
            kind: cyntax_hir::StructTypeKind::Incomplete,
        };
        self.map.tags.insert(id, self.arena.alloc(struct_ty));

        if let Some(tag) = &specifier.tag {
            if specifier.declarations.is_some() {
                self.define_struct_type(tag, id)?;
            } else {
                return Ok(self.find_struct_in_scope(tag)?);
            }
            // let _ = self.define_struct_type(tag, id);
        }

        if let Some(declarations) = &specifier.declarations {
            let mut fields = vec![];
            for declaration in declarations {
                let base = self.lower_ty_specifiers_qualifiers(&declaration.value.specifier_qualifiers)?.into();
                for struct_declarator in &declaration.value.declarators {
                    let ty = self.lower_ty(&base, &struct_declarator.declarator.as_ref().unwrap())?;
                    let name = struct_declarator.declarator.as_ref().map(|decl| decl.value.get_identifier().expect("curious if this is ever none"));
                    fields.push(&*self.arena.alloc(StructField { ty, identifier: name }));
                }
            }
            let struct_ty = StructType {
                id,
                tag: specifier.tag.clone(),
                kind: StructTypeKind::Complete(self.arena.alloc_slice_copy(&fields)),
            };
            self.map.tags.insert(id, self.arena.alloc(struct_ty));
            // if let Some(tag) = &specifier.tag {
            //     self.define_struct_type(tag, id)?;
            // }
        }
        Ok(id)
    }
    fn lower_ty(&mut self, base: &ParsedDeclarationSpecifiers, declarator: &Spanned<ast::Declarator>) -> PResult<&'hir Ty<'hir>> {
        let id = self.next_id();

        let mut kind = if let TypeSpecifierStateMachine::Typedef(name) = base.specifiers {
            let decl = self.map.typedefs.get(&name).unwrap();
            // cloning here is kinda sad but I don't see a way around it? we need a &ParsedDeclarationSpecifiers since we share it between declarators
            decl.ty.kind.clone()
        } else {
            // cloning here is fine honestly, these types are so cheap
            TyKind::Base(SpecifierQualifiers {
                specifiers: base.specifiers.clone(),
                qualifier: base.qualifier.clone(),
            })
        };

        let mut next = Some(declarator);
        while let Some(next_declarator) = next {
            match &next_declarator.value {
                // Nop
                ast::Declarator::Abstract | ast::Declarator::Identifier(_) => {
                    next = None;
                }

                // *decl
                ast::Declarator::Pointer(ptr_info, inner) => {
                    let mut ptr = Some(&ptr_info.value);
                    while let Some(p) = ptr {
                        kind = TyKind::Pointer(p.type_qualifiers.clone(), Box::new(kind));
                        ptr = p.ptr.as_ref().map(|boxed| &boxed.value);
                    }
                    next = Some(inner.deref());
                }

                // (decl)
                ast::Declarator::Parenthesized(inner) => next = Some(&inner.deref()),

                // decl(int a)
                ast::Declarator::Function(inner, parameter_list) => {
                    let mut parameters = vec![];
                    for param in &parameter_list.parameters {
                        // let spec = DeclarationSpecifierParser::new(param.value.specifiers.iter(), &self.scopes, &mut self.map).parse()?;
                        let spec = self.lower_declaration_ty_specifiers(&param.value.specifiers)?;

                        let ty = self.lower_ty(&spec, param.value.declarator.as_ref().unwrap_or(&Spanned::new(Location::new(), ast::Declarator::Abstract)))?;
                        let name = param.value.declarator.as_ref().map(|declarator| declarator.value.get_identifier().map(|identifier| declarator.location.to_spanned(identifier))).flatten();
                        parameters.push(FunctionParameter { ty, identifier: name });
                    }
                    let parameters: &'hir _ = self.arena.alloc_slice_fill_iter(parameters.into_iter());

                    kind = TyKind::Function { return_ty: Box::new(kind), parameters };
                    next = Some(inner);
                }
                // decl[]
                ast::Declarator::Array {
                    base,
                    has_static: _,
                    has_star: _,
                    type_qualifiers: _,
                    expr,
                } => {
                    kind = TyKind::Array(Box::new(kind), self.lower_expression(expr.as_ref().unwrap())?);
                    next = Some(base);
                }
            }
        }
        let ty: &'hir _ = self.arena.alloc(Ty { id, kind: kind });
        Ok(ty)
    }

    pub fn lower_statement(&mut self, statement: &Spanned<ast::Statement>) -> PResult<&'hir hir::Statement<'hir>> {
        #[allow(unused_variables)]
        let kind = match &statement.value {
            ast::Statement::Labeled(labeled_statement) => todo!(),
            ast::Statement::Compound(block_items) => {
                self.push_scope();
                let mut hir_block_items = vec![];
                for item in block_items {
                    match item {
                        ast::BlockItem::Declaration(declaration) => {
                            hir_block_items.extend(self.lower_declaration(&declaration)?.into_iter().map(|decl| hir::BlockItem::Declaration(decl)));
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
            ast::Statement::Iteration(IterationStatement::While(condition, body)) => hir::StatementKind::While(self.lower_expression(condition)?, self.lower_statement(&body)?),
            ast::Statement::Iteration(IterationStatement::ForLoop { init, condition, update, body }) => {
                let mut block_items = vec![];
                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expression(expression) => {
                            let id = self.next_id();
                            block_items.push(hir::BlockItem::Statement(self.arena.alloc(hir::Statement {
                                id,
                                span: expression.location.clone(),
                                kind: hir::StatementKind::Expression(self.lower_expression(expression)?),
                            })));
                        }
                        ast::ForInit::Declaration(spanned) => {
                            let bi = self.lower_declaration(&spanned)?;
                            block_items.extend(bi.into_iter().map(|declaration| BlockItem::Declaration(declaration)));
                        }
                    }
                }
                let control = if let Some(condition) = condition {
                    self.lower_expression(condition)?
                } else {
                    let id = self.next_id();
                    self.arena.alloc(hir::Expression {
                        id,
                        loc: Location::new(),
                        kind: hir::ExpressionKind::Constant(Location::new().to_spanned(IntConstant {
                            number: "1".to_string(),
                            suffix: constant::Suffix {
                                signed: constant::Signedness::None,
                                width: constant::Width::None,
                            },
                            base: 10,
                        })),
                    })
                };
                let inner = {
                    let id = self.next_id();
                    let mut block_items = vec![];
                    block_items.push(hir::BlockItem::Statement(self.lower_statement(&body)?));

                    if let Some(update) = update {
                        block_items.push(hir::BlockItem::Statement(self.arena.alloc(hir::Statement {
                            id,
                            span: update.location.clone(),
                            kind: hir::StatementKind::Expression(self.lower_expression(update)?),
                        })));
                    }
                    let block_items: &'hir _ = self.arena.alloc_slice_fill_iter(block_items.into_iter());
                    self.arena.alloc(hir::Statement {
                        id,
                        span: body.location.clone(),
                        kind: hir::StatementKind::Compound(block_items),
                    })
                };
                let id = self.next_id();
                block_items.push(BlockItem::Statement(self.arena.alloc(hir::Statement {
                    id,
                    span: statement.location.clone(),
                    kind: hir::StatementKind::While(control, inner),
                })));
                let block_items: &'hir _ = self.arena.alloc_slice_fill_iter(block_items.into_iter());
                hir::StatementKind::Compound(block_items)
            }
            ast::Statement::Iteration(iteration_statement) => todo!(),
            ast::Statement::Goto(spanned) => todo!(),
            ast::Statement::Continue => hir::StatementKind::Continue,
            ast::Statement::Break => hir::StatementKind::Break,
            ast::Statement::Return(Some(expression)) => hir::StatementKind::Return(Some(self.lower_expression(expression)?)),
            ast::Statement::Return(None) => hir::StatementKind::Return(None),
            ast::Statement::If(condition, then, statement1) => {
                let condition = self.lower_expression(condition)?;
                let then = self.lower_statement(then.deref())?;
                if let Some(elze) = statement1 {
                    hir::StatementKind::IfThenElse(condition, then, self.lower_statement(elze.deref())?)
                } else {
                    hir::StatementKind::IfThen(condition, then)

                }
            }
            ast::Statement::Switch(spanned, statement) => todo!(),
            ast::Statement::Error => panic!(),
        };

        let id = self.next_id();
        Ok(self.arena.alloc(hir::Statement { id, span: statement.location.clone(), kind }))
    }
    pub fn lower_expression(&mut self, expression: &Spanned<ast::Expression>) -> PResult<&'hir hir::Expression<'hir>> {
        #[allow(unused_variables)]
        let kind = match &expression.value {
            ast::Expression::Parenthesized(expr) => return self.lower_expression(expr.deref()),

            ast::Expression::Identifier(identifier) => {
                let hir_id = self.find_in_scope(identifier)?;
                hir::ExpressionKind::DeclarationReference(hir_id)
            }
            ast::Expression::IntConstant(constant) => hir::ExpressionKind::Constant(constant.clone()),
            ast::Expression::StringLiteral(literal) => todo!(),
            ast::Expression::BinOp(span!(ast::InfixOperator::Access), expr, field) => {
                if let span!(ast::Expression::Identifier(identifier)) = field.deref() {
                    hir::ExpressionKind::MemberAccess(self.lower_expression(expr.deref())?, identifier.clone())
                } else {
                    panic!();
                }
            }
            ast::Expression::BinOp(op, lhs, rhs) => {
                let lhs = self.lower_expression(lhs.deref())?;
                let rhs = self.lower_expression(rhs.deref())?;
                hir::ExpressionKind::BinaryOp(op.clone(), lhs, rhs)
            }
            ast::Expression::UnaryOp(span!(PrefixOperator::AddressOf), expr) => {
                hir::ExpressionKind::AddressOf(self.lower_expression(expr)?)
            }
            ast::Expression::UnaryOp(span!(PrefixOperator::Dereference), expr) => {
                hir::ExpressionKind::Dereference(self.lower_expression(expr)?)
            }
            ast::Expression::UnaryOp(op, expr) => todo!(),
            ast::Expression::PostfixOp(op, expr) => todo!("{:#?}", op),
            ast::Expression::Cast(type_name, expr) => {
                let base = self.lower_ty_specifiers_qualifiers(&type_name.value.specifier_qualifiers)?;
                let derived = self.lower_ty(&base.into(), &type_name.value.declarator)?;
                let expr = self.lower_expression(expr.deref())?;
                hir::ExpressionKind::Cast(derived, expr)
            }
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
    pub fn define_struct_type(&mut self, tag: &Spanned<Identifier>, id: HirId) -> PResult<()> {
        if let Some(s) = self.scopes.last_mut().unwrap().tags.get(&tag.value) {
            let s = self.map.tags.get(s).unwrap();
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
    // pub fn declare_struct_type(&mut self, tag: &Spanned<Identifier>, id:HirId) -> PResult<()> {
        
    // }
}
