use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{lexer::Punctuator, preprocess::ast::Token};

use super::{ast::*, ParseResult, Parser, L};
#[derive(Debug, Clone, Copy)]
pub enum ParseStage {
    Preprocess,
    Parse,
}
#[derive(Debug, Clone, Copy)]
pub struct ParseExpressionOptions {
    pub min_prec: i32,
    pub comma_viable: bool,
    pub stage: ParseStage,
}
impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> ParseResult<Option<L<Expression>>> {
        let options = ParseExpressionOptions {
            min_prec: 0,
            comma_viable: false,
            stage: ParseStage::Parse,
        };
        self.parse_expression_2(options)
    }
    pub fn parse_expression_2(&mut self, options: ParseExpressionOptions) -> ParseResult<Option<L<Expression>>> {
        let next = match self.peek_token() {
            Ok(next) => next,
            Err(_) => return Ok(None),
        };

        let mut left = if matches!(&next.value, Token::Punctuator(Punctuator::LParen)) {
            self.expect_token(&Token::Punctuator(Punctuator::LParen))?;
            let next2 = self.peek_token()?;
            match &next2.value {
                t if self.is_typename(t) => {
                    let type_name = self.parse_typename()?;
                    self.expect_token_trace(&Token::Punctuator(Punctuator::RParen), line!())?;

                    let inner = self.parse_expression_2(options)?.unwrap();

                    Some(next2.shell().until(&inner).same(Expression::Cast(type_name, Box::new(inner))))
                }
                _ => {
                    let inner = self
                        .parse_expression_2(ParseExpressionOptions {
                            comma_viable: true,
                            min_prec: 0,
                            ..options
                        })?
                        .unwrap();
                    let rparen = self.expect_token_trace(&Token::Punctuator(Punctuator::RParen), line!())?;
                    Some(next.shell().until(&rparen).same(inner.value))
                }
            }
        } else if matches!(next.value, Token::Punctuator(Punctuator::Bang)) {
            let operator_token = self.expect_token(&Token::Punctuator(Punctuator::Bang))?.clone();
            let e = self.parse_expression_2(options)?.unwrap();
            Some(
                operator_token
                    .shell()
                    .until(&e)
                    .same(Expression::UnaryOp(Box::new(e), operator_token.same(UnaryOperator::Negate))),
            )
        } else if matches!(next.value, Token::Keyword(Keyword::Sizeof)) {
            self.expect_token(&Token::Keyword(Keyword::Sizeof))?;
            let lparen = self.expect_token_trace(&Token::Punctuator(Punctuator::LParen), line!())?.shell();
            let type_name = self.parse_typename()?;

            let rparen = self.expect_token_trace(&Token::Punctuator(Punctuator::RParen), line!())?.shell();
            Some(lparen.until(&rparen).same(Expression::SizeOf(type_name)))
        } else if matches!(&next.value, Token::Identifier(_)) {
            let i = self.next_token().unwrap().value.as_identifier().unwrap();
            // let is_pp_define = matches!(options.stage, ParseStage::Preprocess) && i == "defined";
            let is_pp_define = matches!(options.stage, ParseStage::Preprocess);
            if is_pp_define || self.resolve_identifier2(&i) {
                // specifically handle defined expression for macros during pre processing
                if is_pp_define && i == "defined" {
                    let inner = self.parse_identifier()?;
                    Some(next.shell().until(&inner).same(Expression::Defined(inner.value)))
                } else {
                    if self.consume_if_present(&Token::Punctuator(Punctuator::LParen))?.value {
                        let mut arguments = vec![];

                        while let Ok(token) = &self.peek_token() {
                            let mut last_comma_span = None;
                            if matches!(token.value, Token::Punctuator(Punctuator::RParen)) {
                                self.expect_token(&Token::Punctuator(Punctuator::RParen))?;
                                break;
                            }
                            if arguments.len() > 0 {
                                last_comma_span = Some(self.expect_token_trace(&Token::Punctuator(Punctuator::Comma), line!())?).map(|t| t.shell());
                            }
                            let expr = match self.parse_expression_2(options)? {
                                Some(e) => e,
                                None => {
                                    let last_comma = last_comma_span.unwrap();
                                    return Err(Diagnostic::error()
                                        .with_message("trailing comma in call expression")
                                        .with_labels(last_comma.generate_location_labels()));
                                }
                            };

                            arguments.push(expr);
                        }

                        Some(next.same(Expression::Call(i.to_string(), arguments)))
                    } else {
                        Some(next.same(Expression::Identifier(i.to_string())))
                    }
                }
            } else if self.resolve_typedef2(&i) {
                return Err(Diagnostic::error()
                    .with_message("Token is a typename not an identifier")
                    .with_labels(next.generate_location_labels()));
            } else {
                return Err(Diagnostic::error()
                    .with_message("Token is neither a typename nor an identifier")
                    .with_labels(next.generate_location_labels()));
            }
        } else if matches!(&next.value, Token::Constant(_)) {
            let constant = self.next_token().unwrap().value.as_constant().unwrap();
            Some(next.same(Expression::Constant(constant.clone())))
        } else if let Token::StringLiteral(s) = &next.value {
            let token = self.next_token()?;
            Some(token.same(Expression::StringLiteral(s.to_string())))
        } else {
            return Ok(None);
        }
        .unwrap();

        // post fix operator
        while let Ok(Some(expression)) = self.parse_postfix(&left) {
            left = expression;
        }

        // Continually parse `<operator> <expression>`, adding them to the right side of our own expression
        while let Ok(next) = self.peek_token() {
            if let Some(op_prec) = Self::get_token_precedence(&next.value) {
                if op_prec < options.min_prec {
                    break;
                }

                let binary_op = self.parse_binary_operator()?;
                let right = self.parse_expression_2(ParseExpressionOptions { min_prec: op_prec + 1, ..options })?;
                match right {
                    Some(right) => {
                        left = left.shell().until(&right).same(Expression::BinOp(Box::new(left), binary_op, Box::new(right)));
                    }
                    None => {
                        left.shell().same::<Option<()>>(None).until(&binary_op).unwrap_diag(&self.files);
                    }
                }
            } else {
                break;
            }
        }

        // Handle ternary operator
        if self.consume_if_present(&Token::Punctuator(Punctuator::Question)).map(|t| t.value).unwrap_or(false) {
            let tru = self.parse_expression_2(options)?;
            self.expect_token(&Token::Punctuator(Punctuator::Colon))?;
            let fals = self.parse_expression_2(options)?;

            left = left
                .shell()
                .until(&fals.as_ref().unwrap_or(tru.as_ref().unwrap_or(&left)))
                .same(Expression::Conditional(ConditionalExpression {
                    condition: Box::new(left),
                    r#true: Box::new(tru),
                    r#false: Box::new(fals),
                }));
        }
        // if options.comma_viable && self.consume_if_present(&Token::Punctuator(Punctuator::Comma)).map(|t| t.value).unwrap_or(false) {
        //     let next = self.parse_expression_2(ParseExpressionOptions { comma_viable: true, ..options })?.unwrap();
        //     left = match left.value {
        //         Expression::Comma(ref items) => {
        //             let mut items = items.clone();
        //             let loc = left.shell().until(&next);
        //             items.push(left);
        //             items.push(next);
        //             loc.same(Expression::Comma(items))
        //         }
        //         _ => {
        //             let mut items = vec![];
        //             let loc = left.shell().until(&next);
        //             items.push(left);

        //             items.push(next);
        //             loc.same(Expression::Comma(items))
        //         }
        //     }
        // }
        Ok(Some(left))
    }
    pub fn parse_typename(&mut self) -> ParseResult<L<TypeName>> {
        let specifiers = self.parse_specifier_list();

        dbg!(&specifiers);
        let d = self.parse_declarator(false)?;
        let lh = specifiers.first().map(|spec| spec.shell()).unwrap_or(d.shell()).until(&d);
        match d.value.kind {
            DeclaratorKind::Identifier(location_history) => {
                return Err(Diagnostic::error()
                    .with_message("typename declarator must be abstract, not identifier")
                    .with_labels(location_history.generate_location_labels()))
            }
            DeclaratorKind::Declarator(location_history) => {
                return Err(Diagnostic::error()
                    .with_message("typename declarator must be abstract, not declarator")
                    .with_labels(lh.generate_location_labels())
                    .with_labels(location_history.generate_location_labels()))
            }
            DeclaratorKind::Abstract => {}
        }
        // assert!(matches!(d.value.kind, DeclaratorKind::Abstract));

        Ok(lh.same(TypeName {
            specifiers: specifiers,
            declarator: d,
        }))
    }
    pub fn parse_postfix(&mut self, expr: &L<Expression>) -> ParseResult<Option<L<Expression>>> {
        if let Ok(next) = self.peek_token() {
            match next.value {
                Token::Punctuator(Punctuator::Dot) => {
                    self.expect_token(&Token::Punctuator(Punctuator::Dot))?;
                    let identifier = self.parse_identifier()?;
                    Ok(Some(expr.shell().same(Expression::Member(Box::new(expr.clone()), identifier))))
                }
                Token::Punctuator(Punctuator::PlusPlus) => {
                    let operator = self.parse_unary_operator()?;
                    Ok(Some(expr.shell().same(Expression::UnaryOp(Box::new(expr.clone()), operator))))
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }

        // while self.consume_if_present(&Token::Punctuator(Punctuator::Dot))?.value {
        //     let identifier = self.parse_identifier()?;
        //     left = left
        //         .shell()
        //         .until(&identifier)
        //         .same(Expression::Member(Box::new(left), identifier.value));
        // }
    }

    pub fn parse_binary_operator(&mut self) -> ParseResult<L<BinaryOperation>> {
        let next = self.next_token()?;
        Ok(next.same(match next.value {
            Token::Punctuator(Punctuator::Plus) => Ok(BinaryOperation::Add),
            Token::Punctuator(Punctuator::Minus) => Ok(BinaryOperation::Sub),
            Token::Punctuator(Punctuator::Asterisk) => Ok(BinaryOperation::Mul),
            Token::Punctuator(Punctuator::Slash) => Ok(BinaryOperation::Div),
            Token::Punctuator(Punctuator::Percent) => Ok(BinaryOperation::Mod),

            Token::Punctuator(Punctuator::LeftAngle) => Ok(BinaryOperation::Less),
            Token::Punctuator(Punctuator::RightAngle) => Ok(BinaryOperation::Greater),

            Token::Punctuator(Punctuator::LessOrEqual) => Ok(BinaryOperation::LessOrEqual),
            Token::Punctuator(Punctuator::GreatorOrEqual) => Ok(BinaryOperation::GreatorOrEqual),

            Token::Punctuator(Punctuator::Equal) => Ok(BinaryOperation::Assign),
            Token::Punctuator(Punctuator::EqualEqual) => Ok(BinaryOperation::Compare),

            Token::Punctuator(Punctuator::AndAnd) => Ok(BinaryOperation::LogicalAnd),
            Token::Punctuator(Punctuator::OrOr) => Ok(BinaryOperation::LogicalOr),

            Token::Punctuator(Punctuator::PlusEqual) => Ok(BinaryOperation::AssignBySum),
            Token::Punctuator(Punctuator::MinusEqual) => Ok(BinaryOperation::AssignByDifference),
            Token::Punctuator(Punctuator::AsteriskEqual) => Ok(BinaryOperation::AssignByProduct),
            Token::Punctuator(Punctuator::SlashEqual) => Ok(BinaryOperation::AssignByQuotient),
            Token::Punctuator(Punctuator::PercentEqual) => Ok(BinaryOperation::AssignByRemainder),

            Token::Punctuator(Punctuator::And) => Ok(BinaryOperation::BitwiseAnd),
            Token::Punctuator(Punctuator::Or) => Ok(BinaryOperation::BitwiseOr),

            Token::Punctuator(Punctuator::Caret) => Ok(BinaryOperation::BitwiseXor),

            _ => Err(Diagnostic::error()
                .with_message("invalid binary operator")
                .with_labels(vec![Label::primary(next.file_id(), next.location_range())])),
        }?))
    }
    pub fn parse_unary_operator(&mut self) -> ParseResult<L<UnaryOperator>> {
        let next = self.next_token()?;
        Ok(next.same(match next.value {
            Token::Punctuator(Punctuator::PlusPlus) => Ok(UnaryOperator::Increment),

            _ => Err(Diagnostic::error().with_message("invalid unary operator").with_labels(next.generate_location_labels())),
        }?))
    }
    fn get_token_precedence(p: &Token) -> Option<i32> {
        match p {
            Token::Punctuator(Punctuator::Asterisk) | Token::Punctuator(Punctuator::Slash) | Token::Punctuator(Punctuator::Percent) => Some(5),
            Token::Punctuator(Punctuator::Plus) | Token::Punctuator(Punctuator::Minus) => Some(4),
            Token::Punctuator(Punctuator::LeftAngle)
            | Token::Punctuator(Punctuator::RightAngle)
            | Token::Punctuator(Punctuator::LessOrEqual)
            | Token::Punctuator(Punctuator::GreatorOrEqual) => Some(3),

            Token::Punctuator(Punctuator::Equal)
            | Token::Punctuator(Punctuator::PlusEqual)
            | Token::Punctuator(Punctuator::MinusEqual)
            | Token::Punctuator(Punctuator::AsteriskEqual)
            | Token::Punctuator(Punctuator::SlashEqual)
            | Token::Punctuator(Punctuator::PercentEqual) => Some(2),
            Token::Punctuator(Punctuator::EqualEqual) => Some(1),
            Token::Punctuator(Punctuator::And) => Some(0),
            Token::Punctuator(Punctuator::Caret) => Some(-1),
            Token::Punctuator(Punctuator::Or) => Some(-2),
            Token::Punctuator(Punctuator::AndAnd) => Some(-3),
            Token::Punctuator(Punctuator::OrOr) => Some(-4),

            _ => None,
        }
        .map(|x| 100 + x)
    }
}
