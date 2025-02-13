use crate::ast::{BlockStatement, Expression, Identifier, LetStatement, Program, Statement};
use crate::environment::Environment;
use crate::object::{Boolean, Error, Integer, Null, Object, ObjectType, Return};
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

pub fn eval(program: &Program, env: &mut Environment) -> Result<Box<dyn Object>, EvalError> {
    let evaluated = eval_statements(&program.statements, env)?;

    if evaluated.type_() == ObjectType::Return {
        let return_value = evaluated.as_any().downcast_ref::<Return>().unwrap();
        return Ok(return_value.value.clone());
    }

    if evaluated.type_() == ObjectType::Error {
        return Ok(evaluated);
    }

    Ok(evaluated)
}

fn eval_statements(
    stmts: &[Statement],
    env: &mut Environment,
) -> Result<Box<dyn Object>, EvalError> {
    let mut res: Result<Box<dyn Object>, EvalError> = Ok(Box::new(Null {}));
    for stmt in stmts {
        res = eval_statement(stmt, env);

        match res.as_ref() {
            Ok(obj) if obj.type_() == ObjectType::Return || obj.type_() == ObjectType::Error => {
                return res
            }
            Ok(_) => (), // Continue if not return
            Err(_) => return res,
        }
    }

    res
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Result<Box<dyn Object>, EvalError> {
    match stmt {
        Statement::Expression(expr) => eval_expression(&expr.expression, env),
        Statement::Let(ls) => {
            let val = eval_expression(&ls.value, env)?;
            // Return error early
            if val.type_() == ObjectType::Error {
                return Ok(val);
            }

            // Else set the value in the environment
            env.set(ls.name.value.as_str(), val.clone());
            Ok(Box::new(Null::new())) // TODO: What should we actually be returning here?
        }
        Statement::Return(ret) => {
            let val = eval_expression(&ret.return_value, env)?;
            Ok(Box::new(Return { value: val }))
        }
        stmt => Err(EvalError::UnknownIdentifier(format!("{:?}", stmt))),
    }
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> Result<Box<dyn Object>, EvalError> {
    match expr {
        Expression::IntegerLiteralExpr(il) => Ok(Box::new(Integer::new(il.value))),
        Expression::BooleanLiteralExpr(bl) => Ok(Box::new(Boolean::new(bl.value))),

        Expression::IdentifierExpr(ident) => eval_identifier(ident, env),

        Expression::Null => Ok(Box::new(Null::new())),

        Expression::PrefixExpr(pe) => {
            let right = eval_expression(&pe.right, env)?;
            eval_prefix_expression(&pe.operator, right)
        }
        Expression::InfixExpr(ie) => {
            let left = eval_expression(&ie.left, env)?;
            let right = eval_expression(&ie.right, env)?;

            eval_infix_expression(&ie.operator, left, right)
        }
        Expression::IfExpr(ie) => eval_if_expression(ie, env),
        // _ => Ok(Box::new(Null::new()))
        expr => Err(EvalError::UnknownIdentifier(
            expr.token_literal().to_string(),
        )),
    }
}

fn eval_block_statement(
    block: &BlockStatement,
    env: &mut Environment,
) -> Result<Box<dyn Object>, EvalError> {
    let mut res: Result<Box<dyn Object>, EvalError> = Ok(Box::new(Null::new()));
    for stmt in &block.statements {
        res = eval_statement(stmt, env);

        // A block may contain a return value, in which case we should return instantly
        match res.as_ref() {
            Ok(obj) if obj.type_() == ObjectType::Return || obj.type_() == ObjectType::Error => {
                return res
            }
            Ok(_) => (), // Continue if not return
            Err(_) => return res,
        }
    }

    res
}

fn eval_if_expression(
    ie: &crate::ast::IfExpression,
    env: &mut Environment,
) -> Result<Box<dyn Object>, EvalError> {
    let condition = eval_expression(&ie.condition, env)?;
    if is_truthy(condition) {
        if let Some(consequence) = &ie.consequence {
            eval_block_statement(consequence, env)
        } else {
            Ok(Box::new(Null::new()))
        }
    } else if let Some(alternative) = &ie.alternative {
        eval_block_statement(alternative, env)
    } else {
        Ok(Box::new(Null::new()))
    }
}

fn is_truthy(obj: Box<dyn Object>) -> bool {
    match obj.type_() {
        ObjectType::Null => false,
        ObjectType::Boolean => {
            let boolean = obj.as_any().downcast_ref::<Boolean>().unwrap();
            boolean.value
        }
        _ => true,
    }
}

fn eval_identifier(
    ident: &Identifier,
    env: &mut Environment,
) -> Result<Box<dyn Object>, EvalError> {
    match env.get(ident.value.as_str()) {
        Some(obj) => Ok(obj),
        None => Ok(new_error(format!(
            "identifier not found: {}",
            ident.value.as_str()
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
        _ => Ok(new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.type_()
        ))),
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
    } else if left.type_() != right.type_() {
        Ok(new_error(format!(
            "type mismatch: {} {} {}",
            left.type_(),
            operator,
            right.type_()
        )))
    } else {
        Ok(new_error(format!(
            "unknown operator: {} {} {}",
            left.type_(),
            operator,
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
        _ => Ok(new_error(format!(
            "unknown operator: {} {} {}",
            left.type_(),
            operator,
            right.type_()
        ))),
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
        _ => Ok(new_error(format!(
            "unknown operator: {} {} {}",
            left.type_(),
            operator,
            right.type_()
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
        _ => Ok(new_error(format!("unknown operator: !{}", right.type_()))),
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
        _ => Ok(new_error(format!("unknown operator: -{}", right.type_()))),
    }
}

fn new_error(msg: String) -> Box<dyn Object> {
    Box::new(Error { message: msg })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::{Error, Integer, Null, ObjectType};
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
            if !test_integer_object(&evaluated, expected) {
                panic!("Expected: {}, Actual: {:?}", expected, evaluated);
            }
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
            if !test_boolean_object(&evaluated, expected) {
                panic!("Expected: {}, Actual: {:?}", expected, evaluated);
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests: Vec<(&str, Box<dyn Object>)> = vec![
            ("if (true) { 10 }", Box::new(Integer::new(10))),
            ("if (false) { 10 }", Box::new(Null::new())),
            ("if (1) { 10 }", Box::new(Integer::new(10))),
            ("if (1 < 2) { 10 }", Box::new(Integer::new(10))),
            ("if (1 > 2) { 10 }", Box::new(Null::new())),
            ("if (1 > 2) { 10 } else { 20 }", Box::new(Integer::new(20))),
            ("if (1 < 2) { 10 } else { 20 }", Box::new(Integer::new(10))),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected.type_() {
                ObjectType::Integer => {
                    let expected_int = expected.as_any().downcast_ref::<Integer>().unwrap();
                    let actual_int = evaluated.as_any().downcast_ref::<Integer>().unwrap();
                    if !test_integer_object(&evaluated, expected_int.value) {
                        panic!(
                            "Expected: {}, Actual: {}",
                            expected_int.value, actual_int.value
                        );
                    }
                }
                ObjectType::Null => {
                    if !test_null_object(&evaluated) {
                        panic!("Expected: Null, Actual: {:?}", evaluated);
                    }
                }
                _ => panic!("Unknown type"),
            }
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
            if !test_boolean_object(&evaluated, expected) {
                panic!("Expected: {}, Actual: {:?}", expected, evaluated);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1 ) {\n\
                    if (10 > 1 ) {\n\
                        return 10;\n\
                    }\n\
                return 1;\n\
                }",
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if !test_integer_object(&evaluated, expected) {
                panic!("Expected: {}, Actual: {:?}", expected, evaluated);
            }
        }
    }

    #[test]
    fn test_error_handlng() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1 ) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1 ) {\n\
                    if (10 > 1 ) {\n\
                        return true + false;\n\
                    }\n\
                return 1;\n\
                }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        let mut errs = 0;

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            match evaluated.type_() {
                ObjectType::Error => {
                    let err_msg = evaluated.as_any().downcast_ref::<Error>().unwrap();
                    if err_msg.message != expected {
                        errs += 1;
                        println!("For input: {}", input);
                        eprintln!("Expected: {}, Actual: {:?}", expected, err_msg.message);
                    }
                }
                _ => {
                    errs += 1;
                    println!("For input: {}", input);
                    eprintln!("Expected type: Error, got: {:?}", evaluated)
                }
            }
        }

        if errs > 0 {
            panic!("test failed. {} errors found", errs);
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        let mut errs = 0;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if !test_integer_object(&evaluated, expected) {
                errs += 1;
                eprintln!("For input: {}", input);
                eprintln!("Expected: {}, Actual: {:?}", expected, evaluated);
            }
        }

        if errs > 1 {
            panic!("Test failed with {} error", errs);
        }
    }

    // **************************************************** //
    // ***************** Helper functions ***************** //
    // **************************************************** //

    fn test_eval(input: &str) -> Box<dyn Object> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l).expect("Parser creation failed");
        let program = p.parse_program();
        let mut env = Environment::new();

        eval(&program, &mut env).expect("Evaluation failed")
    }

    fn test_integer_object(obj: &Box<dyn Object>, expected: i64) -> bool {
        match obj.type_() {
            ObjectType::Integer => {
                let integer = obj.as_any().downcast_ref::<Integer>().unwrap();
                integer.value == expected
            }
            _ => false,
        }
    }

    fn test_null_object(obj: &Box<dyn Object>) -> bool {
        match obj.type_() {
            ObjectType::Null => true,
            _ => false,
        }
    }

    fn test_boolean_object(obj: &Box<dyn Object>, expected: bool) -> bool {
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
