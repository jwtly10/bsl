use crate::ast::{Expression, Program, Statement};
use crate::object::{Integer, Object};

#[derive(Debug, PartialEq)]
pub enum EvalError {
    Unimplemented,
}

pub fn eval(program: &Program) -> Result<Box<dyn Object>, EvalError> {
    eval_statements(&program.statements)
}

fn eval_statements(stmts: &[Statement]) -> Result<Box<dyn Object>, EvalError> {
    let mut res: Result<Box<dyn Object>, EvalError> = Ok(Box::new(Integer::new(0)));
    for stmt in stmts {
        res = eval_statement(stmt);
    }

    res
}

fn eval_statement(stmt: &Statement) -> Result<Box<dyn Object>, EvalError> {
    match stmt {
        Statement::Expression(expr) => eval_expression(&expr.expression),
        _ => Err(EvalError::Unimplemented),
    }
}

fn eval_expression(expr: &Expression) -> Result<Box<dyn Object>, EvalError> {
    match expr {
        Expression::IntegerLiteralExpr(il) => Ok(Box::new(Integer::new(il.value))),
        _ => Err(EvalError::Unimplemented),
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
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
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
}
