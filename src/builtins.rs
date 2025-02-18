use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::object::{new_error, BuiltIn, Integer, Null, Object, ObjectType, StringLit};

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Box<dyn Object>> = {
        let mut m = HashMap::new();
        m.insert(
            "len",
            Box::new(BuiltIn::new(builtin_len)) as Box<dyn Object>,
        );
        m.insert(
            "print",
            Box::new(BuiltIn::new(builtin_print)) as Box<dyn Object>,
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
        _ => new_error(format!(
            "argument to 'len' not supported, got {}",
            args[0].type_()
        )),
    }
}

fn builtin_print(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    Box::new(Null::new())
}
