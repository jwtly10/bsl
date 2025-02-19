use std::collections::HashMap;

use crate::object::{new_error, Array, BuiltIn, Integer, Null, Object, ObjectType, StringLit};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Box<dyn Object>> = {
        let mut m = HashMap::new();
        m.insert(
            "len",
            Box::new(BuiltIn::new(builtin_len)) as Box<dyn Object>,
        );
        m.insert(
            "first",
            Box::new(BuiltIn::new(builtin_first)) as Box<dyn Object>,
        );
        m.insert(
            "last",
            Box::new(BuiltIn::new(builtin_last)) as Box<dyn Object>,
        );
        m.insert(
            "tail",
            Box::new(BuiltIn::new(builtin_tail)) as Box<dyn Object>,
        );
        m.insert(
            "push",
            Box::new(BuiltIn::new(builtin_push)) as Box<dyn Object>,
        );
        m
    };
}

fn builtin_len(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0].type_() {
        ObjectType::StringLit => {
            let str_lit = args[0].as_any().downcast_ref::<StringLit>().unwrap();
            Box::new(Integer::new(str_lit.value.len() as i64))
        }
        ObjectType::Array => {
            let array = args[0].as_any().downcast_ref::<Array>().unwrap();
            Box::new(Integer::new(array.elements.len() as i64))
        }
        _ => new_error(format!(
            "argument to 'len' not supported, got {}",
            args[0].type_()
        )),
    }
}

fn builtin_first(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0].type_() {
        ObjectType::Array => {
            let array = args[0].as_any().downcast_ref::<Array>().unwrap();
            if array.elements.is_empty() {
                return Box::new(Null::new());
            }
            array.elements[0].clone_box()
        }
        _ => new_error(format!(
            "argument to 'first' must be ARRAY, got {}",
            args[0].type_()
        )),
    }
}

fn builtin_last(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0].type_() {
        ObjectType::Array => {
            let array = args[0].as_any().downcast_ref::<Array>().unwrap();
            if array.elements.is_empty() {
                return Box::new(Null::new());
            }
            array.elements[array.elements.len() - 1].clone_box()
        }
        _ => new_error(format!(
            "argument to 'last' must be ARRAY, got {}",
            args[0].type_()
        )),
    }
}

fn builtin_tail(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0].type_() {
        ObjectType::Array => {
            let array = args[0].as_any().downcast_ref::<Array>().unwrap();
            if array.elements.is_empty() {
                return Box::new(Null::new());
            }
            let mut new_elements = array.elements.clone();
            new_elements.remove(0);
            Box::new(Array::new(new_elements))
        }

        _ => new_error(format!(
            "argument to 'last' must be ARRAY, got {}",
            args[0].type_()
        )),
    }
}
fn builtin_push(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 2 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }

    if args[0].type_() != ObjectType::Array {
        return new_error(format!(
            "argument to 'push' must be ARRAY, got {}",
            args[0].type_()
        ));
    }

    let arr = args[0].as_any().downcast_ref::<Array>().unwrap();
    let mut new_elements = arr.elements.clone();
    new_elements.push(args[1].clone_box());

    Box::new(Array::new(new_elements))
}
