use cyntax_common::{span, spanned::Spanned};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_parser::{
    ast::{Expression, InfixOperator, PrefixOperator},
    constant::IntConstant,
};

type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;
#[derive(Debug)]
pub struct ConstantEvalutator {}
pub enum Value {
    Int(i64),
}
impl ConstantEvalutator {
    pub fn new() -> Self {
        Self {}
    }
    pub fn evaluate(&mut self, expr: &Spanned<Expression>) -> PResult<Value> {
        match expr {
            span!(Expression::Identifier(identifier)) => Self::not_const(identifier),
            span!(Expression::IntConstant(constant)) => self.constant(constant),
            span!(Expression::StringLiteral(string)) => Self::not_const(string),
            span!(Expression::Parenthesized(expr)) => self.evaluate(expr),
            span!(Expression::BinOp(op, lhs, rhs)) => self.bin_op(op, lhs, rhs),
            span!(Expression::UnaryOp(op, expr)) => self.un_op(op, expr),
            span!(Expression::PostfixOp(op, xpr)) => todo!(),
            span!(Expression::Cast(ty, expr)) => todo!(),
            span!(Expression::Call(target, paremeters)) => todo!(),
            span!(Expression::Subscript(spanned, spanned1)) => todo!(),
            span!(Expression::Defined(spanned)) => todo!(),
        }
    }
    fn not_const<T>(x: &Spanned<T>) -> PResult<Value> {
        Err(SimpleError(x.location.clone(), "not constant".to_string()).into_codespan_report())
    }
    fn constant(&mut self, constant: &Spanned<IntConstant>) -> PResult<Value> {
        let val = i64::from_str_radix(&constant.value.number, constant.value.base.into()).unwrap();
        Ok(Value::Int(val))
    }
    fn bin_op(&mut self, op: &Spanned<InfixOperator>, left: &Spanned<Expression>, right: &Spanned<Expression>) -> PResult<Value> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match op {
            span!(InfixOperator::Add) => Ok(left.add(right)?),
            span!(InfixOperator::Subtract) => todo!(),
            span!(InfixOperator::Multiply) => todo!(),
            span!(InfixOperator::Divide) => todo!(),
            span!(InfixOperator::Modulo) => todo!(),
            span!(InfixOperator::Less) => todo!(),
            span!(InfixOperator::Greater) => todo!(),
            span!(InfixOperator::LessEqual) => todo!(),
            span!(InfixOperator::GreaterEqual) => todo!(),
            span!(InfixOperator::Equal) => Ok(left.equal(right)?),
            span!(InfixOperator::NotEqual) => todo!(),
            span!(InfixOperator::LogicalAnd) => todo!(),
            span!(InfixOperator::LogicalOr) => todo!(),
            span!(InfixOperator::BitwiseAnd) => todo!(),
            span!(InfixOperator::BitwiseOr) => todo!(),
            span!(InfixOperator::BitwiseXor) => todo!(),
            span!(InfixOperator::BitwiseShiftLeft) => todo!(),
            span!(InfixOperator::BitwiseShiftRight) => todo!(),
            span!(InfixOperator::Assign) => todo!(),
            span!(InfixOperator::AddAssign) => todo!(),
            span!(InfixOperator::SubtractAssign) => todo!(),
            span!(InfixOperator::MultiplyAssign) => todo!(),
            span!(InfixOperator::DivideAssign) => todo!(),
            span!(InfixOperator::ModuloAssign) => todo!(),
            span!(InfixOperator::BitwiseAndAssign) => todo!(),
            span!(InfixOperator::BitwiseOrAssign) => todo!(),
            span!(InfixOperator::BitwiseXorAssign) => todo!(),
            span!(InfixOperator::BitwiseShiftRightAssign) => todo!(),
            span!(InfixOperator::BitwiseShiftLeftAssign) => todo!(),
            span!(InfixOperator::Access) => todo!(),
            span!(InfixOperator::IndirectAccess) => todo!(),
        }
    }
    fn un_op(&mut self, op: &Spanned<PrefixOperator>, expr: &Spanned<Expression>) -> PResult<Value> {
        match op.value{
            span!(PrefixOperator::Plus) => todo!(),
            span!(PrefixOperator::Minus) => todo!(),
            span!(PrefixOperator::LogicalNot) => todo!(),
            span!(PrefixOperator::BitwiseNot) => todo!(),
            span!(PrefixOperator::SizeOf) => todo!(),
            span!(PrefixOperator::CastOrParen) => todo!(),
            span!(PrefixOperator::Dereference) => todo!(),
            span!(PrefixOperator::Increment) => todo!(),
            span!(PrefixOperator::Decrement) => todo!(),
        }
    }
}
impl Value {
    pub fn add(self, other: Self) -> PResult<Self> {
        match (self, other) {
            (Value::Int(lv), Value::Int(rv)) => Ok(Value::Int(lv + rv)),
        }
    }
    pub fn equal(self, other: Self) -> PResult<Self> {
        match (self, other) {
            (Value::Int(lv), Value::Int(rv)) => Ok(if lv == rv { Value::Int(1) } else { Value::Int(0) }),
        }
    }
}
