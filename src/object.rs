use std::any::Any;

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    StringLit,

    BuiltIn,

    Array,

    Return,
    Function,
    Error,
    Null,
}

impl std::fmt::Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::StringLit => write!(f, "STRING"),

            ObjectType::BuiltIn => write!(f, "BUILT_IN"),

            ObjectType::Array => write!(f, "ARRAY"),

            ObjectType::Return => write!(f, "RETURN_VALUE"),
            ObjectType::Function => write!(f, "FUNCTION"),
            ObjectType::Error => write!(f, "ERROR"),
            ObjectType::Null => write!(f, "NULL"),
        }
    }
}

pub trait Object: Any + std::fmt::Debug + Send + Sync {
    fn type_(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;

    fn clone_box(&self) -> Box<dyn Object>;
}

impl Clone for Box<dyn Object> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

pub type BuiltInFunc = fn(Vec<Box<dyn Object>>) -> Box<dyn Object>;

#[derive(Debug, Clone)]
pub struct BuiltIn {
    pub func: BuiltInFunc,
}

impl Object for BuiltIn {
    fn type_(&self) -> ObjectType {
        ObjectType::BuiltIn
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl BuiltIn {
    pub fn new(func: BuiltInFunc) -> Self {
        BuiltIn { func }
    }

    pub fn call(&self, args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
        (self.func)(args)
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Box<dyn Object>>,
}

impl Object for Array {
    fn type_(&self) -> ObjectType {
        ObjectType::Array
    }

    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Array {
    pub fn new(elements: Vec<Box<dyn Object>>) -> Self {
        Array { elements }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn type_(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Integer { value }
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn type_(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Boolean { value }
    }
}

#[derive(Debug, Clone)]
pub struct StringLit {
    pub value: String,
}

impl StringLit {
    pub fn new(value: String) -> Self {
        StringLit { value }
    }
}

impl Object for StringLit {
    fn type_(&self) -> ObjectType {
        ObjectType::StringLit
    }

    fn inspect(&self) -> String {
        self.value.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Box<dyn Object>,
}

impl Object for Return {
    fn type_(&self) -> ObjectType {
        ObjectType::Return
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Object for Function {
    fn type_(&self) -> ObjectType {
        ObjectType::Function
    }

    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.string()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body.string())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Self {
        Function {
            parameters,
            body,
            env,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn type_(&self) -> ObjectType {
        ObjectType::Error
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct Null {}

impl Object for Null {
    fn type_(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Null {
    pub fn new() -> Self {
        Null {}
    }
}

pub fn new_error(msg: String) -> Box<dyn Object> {
    Box::new(Error { message: msg })
}
