// use std::{collections::HashMap, ops::Deref};

// use cyntax_common::spanned::Location;
// use cyntax_errors::{Diagnostic, Label, errors::SimpleError};
// use cyntax_hir::{self as hir, Expression, HirId, SpecifierQualifiers, Ty, TyKind, TyQualifiers, TypeSpecifierStateMachine};
// use cyntax_parser::constant::{Signedness, Suffix, Width};

// use crate::{AstLower, visit::Visitor};

// pub struct TyCheckVisitor<'src, 'ctx, 'hir: 'ctx> {
//     map: HashMap<HirId, &'hir Ty<'hir>>,
//     lowering: &'ctx mut AstLower<'src, 'hir>,
//     pub diagnostics: Vec<cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>,
// }

// impl<'src, 'ctx, 'hir> TyCheckVisitor<'src, 'ctx, 'hir> {
//     pub fn new(ctx: &'ctx mut AstLower<'src, 'hir>) -> Self {
//         Self {
//             map: HashMap::new(),
//             lowering: ctx,
//             diagnostics: vec![],
//         }
//     }
//     pub fn equal(left: &TyKind<'hir>, right: &TyKind<'hir>) -> bool {
//         match (&left, &right) {
//             (TyKind::Base(lhs_specifier_qualifiers), TyKind::Base(rhs_specifier_qualifiers)) => {
//                 dbg!(&lhs_specifier_qualifiers, rhs_specifier_qualifiers);
//                 lhs_specifier_qualifiers.specifiers == rhs_specifier_qualifiers.specifiers
//             }
//             (TyKind::Base(lhs_specifier_qualifiers), _) => false,
//             (TyKind::Pointer(lhs_spanneds, lhs_ty_kind), TyKind::Pointer(spanneds, ty_kind)) => Self::equal(lhs_ty_kind.deref(), ty_kind.deref()),
//             (TyKind::Pointer(lhs_spanneds, lhs_ty_kind), _) => false,
//             (
//                 TyKind::Function {
//                     return_ty: lhs_return_ty,
//                     parameters: lhs_parameters,
//                 },
//                 TyKind::Function { return_ty, parameters },
//             ) => todo!(),
//             (
//                 TyKind::Function {
//                     return_ty: lhs_return_t,
//                     parameters: lhs_parameters,
//                 },
//                 _,
//             ) => todo!(),
//             (TyKind::Array(lhs_ty_kind, lhs_expression), _) => false,
//             (TyKind::Array(lhs_ty_kind, lhs_expression), TyKind::Array(ty_kind, expression)) => todo!(),
//         }
//     }
// }

// impl<'src, 'ctx, 'hir> Visitor<'hir> for TyCheckVisitor<'src, 'ctx, 'hir> {
//     fn visit_translation_unit(&mut self, tu: &'hir cyntax_hir::TranslationUnit<'hir>) {
//         for declaration in tu.declarations {
//             self.visit_external_declaration(declaration)
//         }
//     }

//     fn visit_external_declaration(&mut self, ed: &'hir cyntax_hir::ExternalDeclaration<'hir>) {
//         match ed {
//             cyntax_hir::ExternalDeclaration::FunctionDefinition(function_definition) => self.visit_function_definition(function_definition),
//             cyntax_hir::ExternalDeclaration::Declaration(declaration) => {
//                 self.visit_declaration(declaration);
//             }
//         }
//     }
//     fn visit_declaration(&mut self, decl: &'hir hir::Declaration<'hir>) {
//         let decl_ty = decl.ty;
//         if let Some(init) = decl.init {
//             match init {
//                 cyntax_hir::Initializer::Assignment(expression) => {
//                     self.visit_expression(expression);
//                     let expr_ty = self.map.get(&expression.id).unwrap();
//                     if !Self::equal(&decl_ty.kind, &expr_ty.kind) {
//                         self.diagnostics.push(
//                             MismatchedTypeErr {
//                                 location: &decl.full_location,
//                                 left: decl_ty,
//                                 right: expr_ty,
//                                 left_loc: &decl.declarator_loc,
//                                 right_loc: &expression.loc,
//                             }
//                             .into_codespan_report(),
//                         );
//                     }
//                 }
//             }
//         }
//         let sq = decl.ty.kind.get_specifier_qualifier();
//         if let TypeSpecifierStateMachine::StructOrUnion(struct_id) = sq.specifiers {
//             let sty = self.lowering.map.tags.get(&struct_id).unwrap();

//             match sty.kind {
//                 cyntax_hir::StructTypeKind::Incomplete => {
//                     self.diagnostics.push(SimpleError(decl.full_location.clone(), "incomplete type".to_string()).into_codespan_report());
//                 }
//                 cyntax_hir::StructTypeKind::Complete(struct_fields) => {}
//             }
//         }
//     }
//     fn visit_function_definition(&mut self, fd: &'hir cyntax_hir::FunctionDefinition<'hir>) {
//         self.visit_statement(fd.body)
//     }

//     fn visit_statement(&mut self, stmt: &'hir cyntax_hir::Statement<'hir>) {
//         match &stmt.kind {
//             cyntax_hir::StatementKind::Compound(block_items) => {
//                 for block_item in *block_items {
//                     match block_item {
//                         cyntax_hir::BlockItem::Declaration(declaration) => {
//                             self.visit_declaration(declaration);
//                             // if let Some(hir::Initializer::Assignment(expr)) = declaration.init {
//                             //     self.visit_expression(expr);
//                             // }
//                         }
//                         cyntax_hir::BlockItem::Statement(statement) => self.visit_statement(statement),
//                     }
//                 }
//             }
//             cyntax_hir::StatementKind::Expression(expression) => self.visit_expression(expression),
//             cyntax_hir::StatementKind::Return(expression) => {
//                 if let Some(expression) = expression {
//                     self.visit_expression(expression)
//                 }
//             }
//             cyntax_hir::StatementKind::While(expression, statement) => {
//                 self.visit_expression(expression);
//                 self.visit_statement(&statement)
//             }
//             cyntax_hir::StatementKind::Continue => {}
//             cyntax_hir::StatementKind::Break => {}
//             cyntax_hir::StatementKind::IfThen(expression, statement) => {
//                 self.visit_expression(&expression);
//                 self.visit_statement(&statement);
//             }
//             cyntax_hir::StatementKind::IfThenElse(expression, statement, statement1) => {
//                 self.visit_expression(&expression);
//                 self.visit_statement(&statement);
//                 self.visit_statement(&statement1);
//             }
//         }
//     }

//     fn visit_expression(&mut self, expr: &'hir cyntax_hir::Expression<'hir>) {
//         match &expr.kind {
//             cyntax_hir::ExpressionKind::Constant(constant) => {
//                 let kind = match constant.value.suffix {
//                     Suffix { signed: Signedness::None, width: Width::None } => TyKind::Base(hir::SpecifierQualifiers {
//                         specifiers: hir::TypeSpecifierStateMachine::Int,
//                         qualifier: TyQualifiers { conzt: false, restrict: false, volatile: false },
//                     }),
//                     _ => todo!(),
//                 };
//                 let id = self.lowering.next_id();
//                 let ty = hir::Ty { id, kind };
//                 let ty = self.lowering.arena.alloc(ty);

//                 self.map.insert(expr.id, ty);
//             }
//             cyntax_hir::ExpressionKind::BinaryOp(op, left, right) => {
//                 self.visit_expression(left);
//                 self.visit_expression(right);

//                 let lty = self.map.get(&left.id).unwrap();
//                 let rty = self.map.get(&right.id).unwrap();

//                 if Self::equal(&lty.kind, &rty.kind) {
//                     self.map.insert(expr.id, &lty);
//                 } else {
//                     self.diagnostics.push(
//                         MismatchedTypeErr {
//                             location: &expr.loc,
//                             left_loc: &left.loc,
//                             right_loc: &right.loc,
//                             left: lty,
//                             right: rty,
//                         }
//                         .into_codespan_report(),
//                     );
//                 }
//             }
//             cyntax_hir::ExpressionKind::DeclarationReference(decl_id) => {
//                 let ty = self.lowering.map.ordinary.get(decl_id).unwrap().ty;
//                 self.map.insert(expr.id, ty);
//             }
//             cyntax_hir::ExpressionKind::Cast(ty, expression) => {
//                 self.visit_expression(&expression);
//                 self.map.insert(expr.id, &ty);
//             }
//             cyntax_hir::ExpressionKind::MemberAccess(target, spanned) => {
//                 self.visit_expression(target);
//                 let target_ty = self.map.get(&target.id).unwrap();
//                 match &target_ty.kind {
//                     TyKind::Base(SpecifierQualifiers {
//                         specifiers: TypeSpecifierStateMachine::StructOrUnion(hir_id),
//                         qualifier,
//                     }) => {
//                         let struct_ty = self.lowering.map.tags.get(hir_id).unwrap();
//                         match struct_ty.kind {
//                             cyntax_hir::StructTypeKind::Incomplete => {
//                                 self.diagnostics.push(SimpleError(expr.loc.clone(), format!("cannot access member of incomplete type {:?}", struct_ty)).into_codespan_report());
//                             }
//                             cyntax_hir::StructTypeKind::Complete(struct_fields) => {
//                                 for field in struct_fields {
//                                     if let Some(field_identifier) = field.identifier {
//                                         if field_identifier == spanned.value {
//                                             self.map.insert(expr.id, field.ty);
//                                             break;
//                                         }
//                                     }
//                                 }
//                             }
//                         }
//                     }
//                     _ => {
//                         self.diagnostics.push(SimpleError(expr.loc.clone(), format!("cannot access member of type {}", target_ty)).into_codespan_report());
//                         panic!();
//                     }
//                 }
//             }
//             cyntax_hir::ExpressionKind::AddressOf(expression) => {
//                 self.visit_expression(&expression);
//             }
//             cyntax_hir::ExpressionKind::Dereference(expression) => {
//                 self.visit_expression(&expression);
//             }
//             cyntax_hir::ExpressionKind::Call(expression, expressions) => {
//                 self.visit_expression(&expression);
//                 for expr in expressions {
//                     self.visit_expression(expr);
//                 }
//             },
//         }
//     }
// }
// struct MismatchedTypeErr<'err, 'hir> {
//     location: &'err Location,
//     left: &'hir Ty<'hir>,
//     right: &'hir Ty<'hir>,

//     left_loc: &'err Location,
//     right_loc: &'err Location,
// }
// impl<'b, 'hir> Diagnostic for MismatchedTypeErr<'b, 'hir> {
//     fn title<'a>(&self) -> &'a str {
//         "mismatched type"
//     }

//     fn severity(&self) -> cyntax_errors::DiagnosticSeverity {
//         cyntax_errors::DiagnosticSeverity::Error
//     }
//     fn labels(&self) -> Vec<cyntax_errors::Label> {
//         let mut labels = vec![];

//         labels.push(Label {
//             kind: cyntax_errors::LabelKind::Primary,
//             location: self.location.clone(),
//             message: format!("mismatched types"),
//         });

//         labels.push(Label {
//             kind: cyntax_errors::LabelKind::Secondary,
//             location: self.left_loc.clone(),
//             message: format!("declarator has type {}", self.left),
//         });
//         labels.push(Label {
//             kind: cyntax_errors::LabelKind::Secondary,
//             location: self.right_loc.clone(),
//             message: format!("expression has type {}", self.right),
//         });

//         labels
//     }
// }
