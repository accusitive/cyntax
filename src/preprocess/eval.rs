use std::ops::Deref;

use codespan_reporting::diagnostic::Diagnostic;

use crate::{
    location::LocationHistory,
    parser::ast::{BinaryOperation, Expression},
};

use super::{
    ast::{Constant, IntegerConstantBase},
    PPResult, PreProcessor,
};
impl PreProcessor {
    pub fn evaluate_constant_expression(&mut self, e: &LocationHistory<Expression>) -> PPResult<i64> {
        Self::assert_expression_constant(&e)?;
        match &e.value {
            Expression::Constant(Constant::Integer(int_constant)) => {
                let radix = match int_constant.base {
                    IntegerConstantBase::Octal => 8,
                    IntegerConstantBase::Decimal => 10,
                    IntegerConstantBase::Hexadecimal => 16,
                };

                Ok(i64::from_str_radix(&int_constant.digits, radix).unwrap())
            }
            // Expression::Number(n) => Ok(i64::from_str_radix(&n, 10).unwrap()),
            Expression::BinOp(lhs, op, rhs) => {
                let lhs = self.evaluate_constant_expression(&lhs)?;
                // Short circuiting
                match op.value {
                    BinaryOperation::LogicalAnd => {
                        if !(lhs != 0) {
                            return Ok(0);
                        }
                    }
                    BinaryOperation::LogicalOr => {
                        if lhs != 0 {
                            return Ok(1);
                        }
                    }
                    _ => {}
                }
                let rhs = self.evaluate_constant_expression(&rhs)?;

                let val = match op.value {
                    BinaryOperation::Add => Ok(lhs + rhs),
                    BinaryOperation::Sub => Ok(lhs - rhs),
                    BinaryOperation::Mul => Ok(lhs * rhs),
                    BinaryOperation::Div => Ok(lhs / rhs),
                    BinaryOperation::Mod => Ok(lhs % rhs),
                    BinaryOperation::Less => Ok((lhs < rhs) as i64),
                    BinaryOperation::Greater => Ok((lhs > rhs) as i64),
                    BinaryOperation::LessOrEqual => Ok((lhs <= rhs) as i64),
                    BinaryOperation::GreatorOrEqual => Ok((lhs >= rhs) as i64),
                    BinaryOperation::Compare => Ok((lhs == rhs) as i64),
                    BinaryOperation::LogicalAnd => Ok(((lhs != 0) && (rhs != 0)) as i64),
                    BinaryOperation::LogicalOr => Ok(((lhs != 0) || (rhs != 0)) as i64),
                    BinaryOperation::BitwiseAnd => Ok(lhs & rhs),
                    BinaryOperation::BitwiseXor => Ok(lhs ^ rhs),
                    BinaryOperation::BitwiseOr => Ok(lhs | rhs),

                    BinaryOperation::Assign
                    | BinaryOperation::AssignBySum
                    | BinaryOperation::AssignByDifference
                    | BinaryOperation::AssignByProduct
                    | BinaryOperation::AssignByQuotient
                    | BinaryOperation::AssignByRemainder => Err(Diagnostic::error()
                        .with_message("Invalid Operator in the context of constant expression evaluation")
                        .with_labels(op.generate_location_labels())),
                }?;
                Ok(val)
            }
            Expression::UnaryOp(expr, op) => {
                let val = self.evaluate_constant_expression(expr)?;

                match op.value {
                    crate::parser::ast::UnaryOperator::Negate => Ok( if val != 0 { 0 } else { 1 }),
                    _ => unimplemented!(),
                }
            }
            Expression::Defined(macro_name) => match self.macros.get(macro_name) {
                Some(_) => Ok(1),
                None => Ok(0),
            },
            // if an identifier slips through, its considered to be an undefined macro, and undefined macros expand to 0 in c
            Expression::Identifier(_) => Ok(0),
            Expression::Conditional(conditional_expression) => {
                let cond = self.evaluate_constant_expression(&conditional_expression.condition)? != 0;

                if cond {
                    let t = conditional_expression.r#true.as_ref().clone().unwrap();

                    Ok(self.evaluate_constant_expression(&t)?)
                } else {
                    let f = conditional_expression.r#false.as_ref().clone().unwrap();

                    Ok(self.evaluate_constant_expression(&f)?)
                }
            }
            Expression::SizeOf(_) => {
                // TODO: write code to turn a list of type specifiers into a proper type, then use the proper type to derive SizeOf
                Ok(64)
            }
            _ => unreachable!(),
        }
    }
    pub fn assert_expression_constant(e: &LocationHistory<Expression>) -> PPResult<()> {
        match &e.value {
            Expression::Constant(_) => Ok(()),
            Expression::BinOp(lhs, op, rhs) => {
                Self::assert_expression_constant(&lhs)?;
                match op.value {
                    BinaryOperation::LogicalAnd => {}
                    BinaryOperation::LogicalOr => {}
                    _ => {
                        Self::assert_expression_constant(&rhs)?;
                    }
                }

                Ok(())
            }
            Expression::UnaryOp(lhs, _) => {
                Self::assert_expression_constant(&lhs)?;
                Ok(())
            }
            Expression::Defined(_) => Ok(()),
            // undefined macros show up here as idenitifer, this is fine technically
            Expression::Identifier(_) => Ok(()),
            Expression::Conditional(conditional_expression) => {
                Self::assert_expression_constant(&conditional_expression.condition)?;
                if let Some(t) = conditional_expression.r#true.deref() {
                    Self::assert_expression_constant(&t)?;
                }
                if let Some(f) = conditional_expression.r#false.deref() {
                    Self::assert_expression_constant(&f)?;
                }
                Ok(())
            }
            Expression::SizeOf(_) => Ok(()),
            _ => Err(Diagnostic::error()
                .with_message(format!("Not a constant expression. {:?}", &e.value))
                .with_labels(e.generate_location_labels())),
        }
    }
}
