use crate::ast::{Expression, Program, Statement};
use crate::object::{Boolean, Integer, Null, Object, ObjectType};
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnknownIdentifier(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnknownIdentifier(ident) => write!(f, "Unknown identifier: {}", ident),
        }
    }
}

pub fn eval(program: &Program) -> Result<Box<dyn Object>, EvalError> {
    eval_statements(&program.statements)
}

fn eval_statements(stmts: &[Statement]) -> Result<Box<dyn Object>, EvalError> {
    let mut res: Result<Box<dyn Object>, EvalError> = Ok(Box::new(Null {}));
    for stmt in stmts {
        res = eval_statement(stmt);
    }

    res
}

fn eval_statement(stmt: &Statement) -> Result<Box<dyn Object>, EvalError> {
    match stmt {
        Statement::Expression(expr) => eval_expression(&expr.expression),
        stmt => Err(EvalError::UnknownIdentifier(format!("{:?}", stmt))),
    }
}

fn eval_expression(expr: &Expression) -> Result<Box<dyn Object>, EvalError> {
    match expr {
        Expression::IntegerLiteralExpr(il) => Ok(Box::new(Integer::new(il.value))),
        Expression::BooleanLiteralExpr(bl) => Ok(Box::new(Boolean::new(bl.value))),
        Expression::Null => Ok(Box::new(Null::new())),

        Expression::PrefixExpr(pe) => {
            let right = eval_expression(&pe.right)?;
            eval_prefix_expression(&pe.operator, right)
        }
        Expression::InfixExpr(ie) => {
            let left = eval_expression(&ie.left)?;
            let right = eval_expression(&ie.right)?;

            eval_infix_expression(&ie.operator, left, right)
        }

        expr => Err(EvalError::UnknownIdentifier(format!(
            "{}",
            expr.token_literal()
        ))),
    }
}

fn eval_prefix_expression(
    operator: &str,
    right: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvalError::UnknownIdentifier(operator.to_string())),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    // Check left and right type are int
    if left.type_() == ObjectType::Integer && right.type_() == ObjectType::Integer {
        let left = left.as_any().downcast_ref::<Integer>().unwrap();
        let right = right.as_any().downcast_ref::<Integer>().unwrap();

        eval_integer_infix_expression(operator, left, right)
    } else if left.type_() == ObjectType::Boolean && right.type_() == ObjectType::Boolean {
        let left = left.as_any().downcast_ref::<Boolean>().unwrap();
        let right = right.as_any().downcast_ref::<Boolean>().unwrap();

        eval_boolean_infix_expression(operator, left, right)
    } else {
        Err(EvalError::UnknownIdentifier(format!(
            "Infix operator not supported for {:?} and {:?}",
            left.type_(),
            right.type_()
        )))
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &Integer,
    right: &Integer,
) -> Result<Box<dyn Object>, EvalError> {
    match operator {
        "+" => Ok(Box::new(Integer::new(left.value + right.value))),
        "-" => Ok(Box::new(Integer::new(left.value - right.value))),
        "*" => Ok(Box::new(Integer::new(left.value * right.value))),
        "/" => Ok(Box::new(Integer::new(left.value / right.value))),
        "<" => Ok(Box::new(Boolean::new(left.value < right.value))),
        ">" => Ok(Box::new(Boolean::new(left.value > right.value))),
        "==" => Ok(Box::new(Boolean::new(left.value == right.value))),
        "!=" => Ok(Box::new(Boolean::new(left.value != right.value))),
        _ => Err(EvalError::UnknownIdentifier(operator.to_string())),
    }
}

fn eval_boolean_infix_expression(
    operator: &str,
    left: &Boolean,
    right: &Boolean,
) -> Result<Box<dyn Object>, EvalError> {
    match operator {
        "==" => Ok(Box::new(Boolean::new(left.value == right.value))),
        "!=" => Ok(Box::new(Boolean::new(left.value != right.value))),
        _ => Err(EvalError::UnknownIdentifier(format!(
            "Unsupported operator for type BOOL: {}",
            operator
        ))),
    }
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Result<Box<dyn Object>, EvalError> {
    match right.type_() {
        ObjectType::Boolean => {
            let boolean = right.as_any().downcast_ref::<Boolean>().unwrap();
            Ok(Box::new(Boolean::new(!boolean.value)))
        }
        ObjectType::Integer => {
            let integer = right.as_any().downcast_ref::<Integer>().unwrap();
            Ok(Box::new(Boolean::new(integer.value == 0)))
        }
        ObjectType::Null => Ok(Box::new(Boolean::new(true))),
        _ => Err(EvalError::UnknownIdentifier(format!(
            "Bang operator not supported for {:?}",
            right.type_()
        ))),
    }
}

fn eval_minus_prefix_operator_expression(
    right: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    match right.type_() {
        ObjectType::Integer => {
            let integer = right.as_any().downcast_ref::<Integer>().unwrap();
            Ok(Box::new(Integer::new(-integer.value)))
        }
        _ => Err(EvalError::UnknownIdentifier(format!(
            "Minus operator not supported for {:?}",
            right.type_()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::{Integer, ObjectType};
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    // **************************************************** //
    // ***************** Helper functions ***************** //
    // **************************************************** //

    fn test_eval(input: &str) -> Box<dyn Object> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l).expect("Parser creation failed");
        let program = p.parse_program();

        eval(&program).expect("Evaluation failed")
    }

    fn test_integer_object(obj: Box<dyn Object>, expected: i64) -> bool {
        match obj.type_() {
            ObjectType::Integer => {
                let integer = obj.as_any().downcast_ref::<Integer>().unwrap();
                integer.value == expected
            }
            _ => false,
        }
    }

    fn test_boolean_object(obj: Box<dyn Object>, expected: bool) -> bool {
        match obj.type_() {
            ObjectType::Boolean => {
                let boolean = obj
                    .as_any()
                    .downcast_ref::<crate::object::Boolean>()
                    .unwrap();
                boolean.value == expected
            }
            _ => false,
        }
    }
}
