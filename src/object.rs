use std::any::Any;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

impl std::fmt::Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::Null => write!(f, "NULL"),
        }
    }
}

pub trait Object: Any + std::fmt::Debug {
    fn type_(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
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
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Boolean { value }
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
}
