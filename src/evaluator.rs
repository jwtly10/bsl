use crate::ast::{BlockStatement, Expression, HashLiteral, Identifier, Program, Statement};
use crate::builtins::BUILTINS;
use crate::environment::Environment;
use crate::object::{
    new_error, Array, Boolean, BuiltIn, Function, Hash, HashPair, Hashable, Integer, Null, Object,
    ObjectType, Return, StringLit,
};
use std::collections::HashMap;
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
        Expression::StringLiteralExpr(sl) => Ok(Box::new(StringLit::new(sl.value.clone()))),

        Expression::IdentifierExpr(ident) => eval_identifier(ident, env),

        Expression::ArrayExpr(ae) => {
            let elements = ae
                .elements
                .iter()
                .map(|el| eval_expression(el, env))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Box::new(Array::new(elements)))
        }

        Expression::HashExpr(hl) => eval_hash_literal(hl, env),

        Expression::IndexExpr(ie) => {
            let left = match eval_expression(&ie.left, env) {
                Ok(obj) => obj,
                Err(e) => return Err(e),
            };

            let index = match eval_expression(&ie.index, env) {
                Ok(obj) => obj,
                Err(e) => return Err(e),
            };

            eval_index_expression(left, index)
        }

        Expression::FunctionExpr(fe) => Ok(Box::new(Function::new(
            fe.parameters.clone(),
            fe.body.clone(),
            env.clone(),
        ))),

        Expression::CallExpr(ce) => {
            let function = eval_expression(&ce.function, env)?;
            if function.type_() == ObjectType::Error {
                return Ok(function);
            }

            let args = ce
                .arguments
                .iter()
                .map(|arg| eval_expression(arg, env))
                .collect::<Result<Vec<_>, _>>()?;

            if args.len() == 1 && args[0].type_() == ObjectType::Error {
                return Ok(args[0].clone());
            } else {
                Ok(apply_function(function, args))
            }
        }

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
        expr => Err(EvalError::UnknownIdentifier(
            expr.token_literal().to_string(),
        )),
    }
}

fn eval_index_expression(
    left: Box<dyn Object>,
    index: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    match (left.type_(), index.type_()) {
        (ObjectType::Array, ObjectType::Integer) => {
            let array = left.as_any().downcast_ref::<Array>().unwrap();
            let idx = index.as_any().downcast_ref::<Integer>().unwrap().value as usize;

            if idx >= array.elements.len() {
                return Ok(Box::new(Null::new()));
            }

            Ok(array.elements[idx].clone())
        }
        (ObjectType::Hash, _) => eval_hash_index_expression(left, index),
        _ => Ok(new_error(format!(
            "index operator not supported: {}[{}]",
            left.type_(),
            index.type_()
        ))),
    }
}

fn apply_function(func: Box<dyn Object>, args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    match func.type_() {
        ObjectType::Function => {
            let function = func.as_any().downcast_ref::<Function>().unwrap();
            let mut extended_env = extend_function_env(function.clone(), args);
            let evaluated = eval_block_statement(&function.body, &mut extended_env);

            match evaluated {
                Ok(obj) => {
                    if obj.type_() == ObjectType::Return {
                        return unwrap_return_value(obj);
                    }
                    obj
                }
                Err(_) => evaluated.unwrap(),
            }
        }
        ObjectType::BuiltIn => {
            let function = func.as_any().downcast_ref::<BuiltIn>().unwrap();
            function.call(args)
        }
        _ => new_error(format!("not a function: {}", func.type_())),
    }
}

fn extend_function_env(func: Function, args: Vec<Box<dyn Object>>) -> Environment {
    let mut env = Environment::new_enclosed(func.env.clone());

    for (i, param) in func.parameters.iter().enumerate() {
        env.set(param.value.as_str(), args[i].clone());
    }

    env
}

fn unwrap_return_value(obj: Box<dyn Object>) -> Box<dyn Object> {
    if obj.type_() == ObjectType::Return {
        let return_value = obj.as_any().downcast_ref::<Return>().unwrap();
        return return_value.value.clone();
    }

    obj
}

fn eval_hash_literal(
    node: &HashLiteral,
    env: &mut Environment,
) -> Result<Box<dyn Object>, EvalError> {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in &node.pairs {
        let key = eval_expression(key_node, env)?;

        let hash_key = if let Some(int) = key.as_any().downcast_ref::<Integer>() {
            int.hash_key()
        } else if let Some(str) = key.as_any().downcast_ref::<StringLit>() {
            str.hash_key()
        } else if let Some(bool) = key.as_any().downcast_ref::<Boolean>() {
            bool.hash_key()
        } else {
            return Ok(new_error(format!("unusable as hash key: {}", key.type_())));
        };

        let value = eval_expression(value_node, env)?;

        pairs.insert(hash_key, HashPair::new(key, value));
    }

    Ok(Box::new(Hash { pairs }))
}

fn eval_hash_index_expression(
    hash: Box<dyn Object>,
    index: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    let hash_obj = hash.as_any().downcast_ref::<Hash>().unwrap();

    let hash_key = if let Some(int) = index.as_any().downcast_ref::<Integer>() {
        int.hash_key()
    } else if let Some(str) = index.as_any().downcast_ref::<StringLit>() {
        str.hash_key()
    } else if let Some(bool) = index.as_any().downcast_ref::<Boolean>() {
        bool.hash_key()
    } else {
        return Ok(new_error(format!(
            "unusable as hash key: {}",
            index.type_()
        )));
    };

    match hash_obj.pairs.get(&hash_key) {
        Some(pair) => Ok(pair.value.clone()),
        None => Ok(Box::new(Null::new())),
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
    if env.get(ident.value.as_str()).is_some() {
        return Ok(env.get(ident.value.as_str()).unwrap());
    }

    if BUILTINS.get(ident.value.as_str()).is_some() {
        return Ok(BUILTINS.get(ident.value.as_str()).unwrap().clone());
    }

    Ok(new_error(format!("identifier not found: {}", ident.value)))
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
    } else if left.type_() == ObjectType::StringLit && right.type_() == ObjectType::StringLit {
        let left = left.as_any().downcast_ref::<StringLit>().unwrap();
        let right = right.as_any().downcast_ref::<StringLit>().unwrap();
        eval_string_infix_expression(operator, left, right)
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

fn eval_string_infix_expression(
    operator: &str,
    left: &StringLit,
    right: &StringLit,
) -> Result<Box<dyn Object>, EvalError> {
    match operator {
        "+" => Ok(Box::new(StringLit::new(left.value.clone() + &right.value))),
        _ => Ok(new_error(format!(
            "unknown operator: {} {} {}",
            left.type_(),
            operator,
            right.type_()
        ))),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::{Error, Hashable, Integer, Null, ObjectType, StringLit};
    use crate::parser::Parser;

    pub enum ExpectedValue {
        Integer(i64),
        Boolean(bool),
        StringLit(String),
        Null,
        Error(String),
    }

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
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
            ("len(1)", "argument to 'len' not supported, got INTEGER"),
            (
                "len(\"one\", \"two\")",
                "wrong number of arguments. got=2, want=1",
            ),
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                "unusable as hash key: FUNCTION",
            ),
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

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input);

        match evaluated.type_() {
            ObjectType::Function => {
                let function = evaluated
                    .as_any()
                    .downcast_ref::<crate::object::Function>()
                    .unwrap();
                if function.parameters.len() != 1 {
                    panic!("Expected 1 parameter, got: {}", function.parameters.len());
                }

                if function.parameters[0].value != "x" {
                    panic!(
                        "Expected parameter name: x, got: {}",
                        function.parameters[0].value
                    );
                }

                let expected_body = "(x + 2)";
                if function.body.string() != expected_body {
                    panic!(
                        "Expected body: {}, got: {}",
                        expected_body,
                        function.body.string()
                    );
                }
            }
            _ => panic!("Expected Function, got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) {x;}; identity(5);", 5),
            ("let identity = fn(x) {return x;}; identity(5);", 5),
            ("let double = fn(x) {x * 2;}; double(5);", 10),
            ("let add = fn(x, y) {x + y;}; add(5, 5);", 10),
            ("let add = fn(x, y) {x + y;}; add(5 + 5, add(5, 5));", 20),
            ("fn(x) {x;}(5)", 5),
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

    #[test]
    fn test_closures() {
        let input = "\n\
                     let newAdder = fn(x) {\n\
                            fn(y) { x + y };\n\
                        };\n\
                        let addTwo = newAdder(2);\n\
                        addTwo(2);";

        let evaluated = test_eval(input);

        if !test_integer_object(&evaluated, 4) {
            panic!("Expected 4, got: {:?}", evaluated);
        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";

        let evaluated = test_eval(input);

        match evaluated.type_() {
            ObjectType::StringLit => {
                let string = evaluated.as_any().downcast_ref::<StringLit>().unwrap();
                if string.value != "Hello World!" {
                    panic!("Expected 'Hello World!', got: {}", string.value);
                }
            }
            _ => panic!("Expected String, got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        let evaluated = test_eval(input);

        match evaluated.type_() {
            ObjectType::StringLit => {
                let string = evaluated.as_any().downcast_ref::<StringLit>().unwrap();
                if string.value != "Hello World!" {
                    panic!("Expected 'Hello World!', got: {}", string.value);
                }
            }
            _ => panic!("Expected String, got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_built_in_functions() {
        let tests = vec![
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"hello world\")", 11),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated.type_() {
                ObjectType::Integer => {
                    let integer = evaluated.as_any().downcast_ref::<Integer>().unwrap();
                    if integer.value != expected {
                        panic!("Expected {}, got: {}", expected, integer.value);
                    }
                }
                _ => panic!("Expected Integer, got: {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);

        match evaluated.type_() {
            ObjectType::Array => {
                let array = evaluated
                    .as_any()
                    .downcast_ref::<crate::object::Array>()
                    .unwrap();
                if array.elements.len() != 3 {
                    panic!("Expected 3 elements, got: {}", array.elements.len());
                }

                let expected = vec![1, 4, 6];
                for (i, el) in array.elements.iter().enumerate() {
                    if !test_integer_object(el, expected[i]) {
                        panic!("Expected {}, got: {:?}", expected[i], el);
                    }
                }
            }
            _ => panic!("Expected Array, got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", ExpectedValue::Integer(1)),
            ("[1, 2, 3][1]", ExpectedValue::Integer(2)),
            ("[1, 2, 3][2]", ExpectedValue::Integer(3)),
            ("let i = 0; [1][i];", ExpectedValue::Integer(1)),
            ("[1, 2, 3][1 + 1];", ExpectedValue::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[2];",
                ExpectedValue::Integer(3),
            ),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                ExpectedValue::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                ExpectedValue::Integer(2),
            ),
            ("[1, 2, 3][3]", ExpectedValue::Null),
            ("[1, 2, 3][-1]", ExpectedValue::Null),
        ];

        for (input, expected) in tests {
            match expected {
                ExpectedValue::Integer(expected) => {
                    let evaluated = test_eval(input);
                    if !test_integer_object(&evaluated, expected) {
                        panic!("Expected: {}, Actual: {:?}", expected, evaluated);
                    }
                }
                ExpectedValue::Null => {
                    let evaluated = test_eval(input);
                    if !test_null_object(&evaluated) {
                        panic!("Expected: Null, Actual: {:?}", evaluated);
                    }
                }
                ExpectedValue::Error(msg) => {
                    let evaluated = test_eval(input);
                    if evaluated.type_() != ObjectType::Error {
                        panic!("Expected: Error, Actual: {:?}", evaluated);
                    }

                    let err = evaluated.as_any().downcast_ref::<Error>().unwrap();
                    if err.message != msg {
                        panic!("Expected: {}, Actual: {}", msg, err.message);
                    }
                }
                ExpectedValue::StringLit(s) => {
                    let evaluated = test_eval(input);
                    if evaluated.type_() != ObjectType::StringLit {
                        panic!("Expected: StringLit, Actual: {:?}", evaluated);
                    }

                    let string = evaluated.as_any().downcast_ref::<StringLit>().unwrap();
                    if string.value != s {
                        panic!("Expected: {}, Actual: {}", s, string.value);
                    }
                }
                ExpectedValue::Boolean(b) => {
                    let evaluated = test_eval(input);
                    if !test_boolean_object(&evaluated, b) {
                        panic!("Expected: {}, Actual: {:?}", b, evaluated);
                    }
                }
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "\
            let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }";

        let evaluated = test_eval(input);

        match evaluated.type_() {
            ObjectType::Hash => {
                let hash = evaluated
                    .as_any()
                    .downcast_ref::<crate::object::Hash>()
                    .unwrap();
                let expected = vec![
                    (
                        StringLit::new("one".to_string()).hash_key(),
                        ExpectedValue::Integer(1),
                    ),
                    (
                        StringLit::new("two".to_string()).hash_key(),
                        ExpectedValue::Integer(2),
                    ),
                    (
                        StringLit::new("three".to_string()).hash_key(),
                        ExpectedValue::Integer(3),
                    ),
                    (Integer::new(4).hash_key(), ExpectedValue::Integer(4)),
                    (Boolean::new(true).hash_key(), ExpectedValue::Integer(5)),
                    (Boolean::new(false).hash_key(), ExpectedValue::Integer(6)),
                ];

                if hash.pairs.len() != expected.len() {
                    panic!(
                        "Expected {} pairs, got: {}",
                        expected.len(),
                        hash.pairs.len()
                    );
                }

                for (expectedKey, expectedValue) in expected {
                    let pair = hash.pairs.get(&expectedKey).unwrap();
                    match expectedValue {
                        ExpectedValue::Integer(expected) => {
                            if !test_integer_object(&pair.value, expected) {
                                panic!("Expected: {}, Actual: {:?}", expected, pair.value);
                            }
                        }
                        _ => panic!("Expected Integer, got: {:?}", pair.value),
                    }
                }
            }
            _ => panic!("Expected Hash, got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", ExpectedValue::Integer(5)),
            ("{\"foo\": 5}[\"bar\"]", ExpectedValue::Null),
            (
                "let key = \"foo\"; {\"foo\": 5}[key]",
                ExpectedValue::Integer(5),
            ),
            ("{}[\"foo\"]", ExpectedValue::Null),
            ("{5: 5}[5]", ExpectedValue::Integer(5)),
            ("{true: 5}[true]", ExpectedValue::Integer(5)),
            ("{false: 5}[false]", ExpectedValue::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                ExpectedValue::Integer(expected) => {
                    if !test_integer_object(&evaluated, expected) {
                        panic!("Expected: {}, Actual: {:?}", expected, evaluated);
                    }
                }
                ExpectedValue::Null => {
                    if !test_null_object(&evaluated) {
                        panic!("Expected: Null, Actual: {:?}", evaluated);
                    }
                }
                _ => {
                    panic!(
                        "Expected Integer or null expected values, got: {:?}",
                        evaluated
                    );
                }
            }
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

        // TODO: We need to remove this .expect, as it causes bad error message which
        // we wont be able to feed into repl/interpreter
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
        matches!(obj.type_(), ObjectType::Null)
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
