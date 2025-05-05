use std::ops::Deref;

use cyntax_common::span;
use cyntax_common::spanned::Spanned;
use cyntax_parser::ast::PrefixOperator;
use cyntax_parser::{ast, PResult};
use crate::hir;

use crate::AstLower;

impl<'src, 'hir> AstLower<'src, 'hir> {

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
            ast::Expression::UnaryOp(span!(PrefixOperator::AddressOf), expr) => hir::ExpressionKind::AddressOf(self.lower_expression(expr)?),
            ast::Expression::UnaryOp(span!(PrefixOperator::Dereference), expr) => hir::ExpressionKind::Dereference(self.lower_expression(expr)?),
            ast::Expression::UnaryOp(op, expr) => todo!(),
            ast::Expression::PostfixOp(op, expr) => todo!("{:#?}", op),
            ast::Expression::Cast(type_name, expr) => {
                let base = self.lower_ty_specifiers_qualifiers(&type_name.value.specifier_qualifiers)?;
                let derived = self.lower_ty(&base.into(), &type_name.value.declarator)?;
                let expr = self.lower_expression(expr.deref())?;
                hir::ExpressionKind::Cast(derived, expr)
            }
            ast::Expression::Call(expr, args) => {
                let expr = self.lower_expression(expr.deref())?;
                let args = args.iter().map(|e| self.lower_expression(e)).collect::<PResult<_>>()?;

                hir::ExpressionKind::Call(expr, args)
            },
            ast::Expression::Subscript(expr, offset) => todo!(),
            ast::Expression::Ternary(control, then, elze) => todo!(),
            ast::Expression::Sizeof(type_name) => todo!(),
            ast::Expression::Null => todo!(),
        };
        let id = self.next_id();
        let expr = self.arena.alloc(hir::Expression { id, kind, loc: expression.location.clone() });

        self.map.insert_expression(id, expr);
        Ok(expr)
    }
}