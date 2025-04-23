use std::{collections::HashMap, ops::Deref, ptr};

pub use bumpalo::Bump;
use cyntax_common::{
    ctx::Context,
    span,
    spanned::{Location, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_hir::{self as hir, DerivedTy, HirId, ParsedDeclarationSpecifiers, SpecifierQualifiers, StructField, StructType, StructTypeKind, Ty, TyQualifiers, TypeSpecifierStateMachine};
pub use cyntax_parser::ast;
use cyntax_parser::ast::{DeclarationSpecifier, Identifier, SpecifierQualifier};

pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

#[derive(Debug)]
pub struct AstLower<'src, 'hir> {
    pub ctx: &'src mut Context,
    map: HirMap<'hir>,
    arena: &'hir Bump,
    scopes: Vec<Scope>,
    next_id: usize,
}
#[derive(Debug)]
pub struct HirMap<'hir> {
    // probably expression?
    ordinary: HashMap<HirId, &'hir hir::Declaration<'hir>>,
    // a type probably?
    typedefs: HashMap<HirId, &'hir hir::Declaration<'hir>>,
    // etc
    tags: HashMap<HirId, &'hir hir::StructType<'hir>>,
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
                let declarations: Vec<&'hir _> = self.lower_declaration(declaration)?;
                let external_declarations: Vec<&'hir _> = declarations.into_iter().map(|declaration| &*self.arena.alloc(hir::ExternalDeclaration::Declaration(declaration))).collect::<Vec<_>>();
                Ok(external_declarations)
            }
        }
    }
    pub fn lower_declaration(&mut self, declaration: &ast::Declaration) -> PResult<Vec<&'hir hir::Declaration<'hir>>> {
        let specifiers = self.lower_declaration_ty_specifiers(&declaration.specifiers)?;

        // let parser = DeclarationSpecifierParser::new(declaration.specifiers.iter(), &self.scopes, &mut self.map);
        // let specifiers = parser.parse()?;
        let mut d = vec![];
        for init_declarator in &declaration.init_declarators {
            let lowered_ty = self.lower_ty(&specifiers, &init_declarator.value.declarator)?;

            let id = self.next_id();
            let is_typedef = matches!(&specifiers.class, Some(ast::StorageClassSpecifier::Typedef));
            if let Some(identifier) = init_declarator.value.declarator.value.get_identifier() {
                // self.scopes.last_mut().unwrap().ordinary.insert(identifier, id);
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
                loc: init_declarator.location.clone(),
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

        Ok(d)
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
                        let id = self.lower_struct_ty_specifier(specifier)?;
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
            tag: specifier.tag,
            kind: cyntax_hir::StructTypeKind::Incomplete,
        };
        self.map.tags.insert(id, self.arena.alloc(struct_ty));

        if let Some(declarations) = &specifier.declarations {
            let mut fields = vec![];
            for declaration in declarations {
                let base = self.lower_ty_specifiers_qualifiers(&declaration.value.specifier_qualifiers)?.into();
                for struct_declarator in &declaration.value.declarators {
                    let ty = self.lower_ty(&base, &struct_declarator.declarator.as_ref().unwrap())?;
                    let name = struct_declarator.declarator.as_ref().map(|decl| decl.value.get_identifier().expect("curious if this is ever none"));
                    fields.push(&*self.arena.alloc(StructField {
                        ty,
                        identifier: name
                    }));
                }
            }
            let struct_ty = StructType {
                id,
                tag: specifier.tag,
                kind: StructTypeKind::Complete(self.arena.alloc_slice_copy(&fields)),
            };
            self.map.tags.insert(id, self.arena.alloc(struct_ty));
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
            DerivedTy::Base(SpecifierQualifiers {
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
                        kind = DerivedTy::Pointer(p.type_qualifiers.clone(), Box::new(kind));
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

                        let d = self.lower_ty(&spec, param.value.declarator.as_ref().unwrap_or(&Spanned::new(Location::new(), ast::Declarator::Abstract)))?;
                        parameters.push(d);
                    }
                    let parameters: &'hir _ = self.arena.alloc_slice_fill_iter(parameters.into_iter());

                    kind = DerivedTy::Function { return_ty: Box::new(kind), parameters };
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
                    kind = DerivedTy::Array(Box::new(kind), self.lower_expression(expr.as_ref().unwrap())?);
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
                            hir_block_items.extend(self.lower_declaration(&declaration.value)?.into_iter().map(|decl| hir::BlockItem::Declaration(decl)));
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
        #[allow(unused_variables)]
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
    fn find_typedef_in_scope(&mut self, identifier: &Spanned<Identifier>) -> PResult<HirId> {
        // reversed, because the newest scope has priority over the 2nd newest
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.typedefs.get(&identifier.value) {
                return Ok(id);
            }
        }
        Err(SimpleError(identifier.location.clone(), format!("could not find typedef `{}` in scope", self.ctx.res(identifier.value))).into_codespan_report())
    }
}
