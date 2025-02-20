use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};
use fnv::FnvHasher;
use std::any::Any;
use std::collections::HashMap;
use std::hash::Hasher;

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    StringLit,

    BuiltIn,

    Array,

    Hash,

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

            ObjectType::Hash => write!(f, "HASH"),

            ObjectType::Return => write!(f, "RETURN_VALUE"),
            ObjectType::Function => write!(f, "FUNCTION"),
            ObjectType::Error => write!(f, "ERROR"),
            ObjectType::Null => write!(f, "NULL"),
        }
    }
}

impl std::hash::Hash for ObjectType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
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

#[derive(Debug, Clone)]
pub struct HashKey {
    pub type_: ObjectType,
    pub value: u64,
}

impl std::hash::Hash for HashKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_.hash(state);
        self.value.hash(state);
    }
}

impl PartialEq for HashKey {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_ && self.value == other.value
    }
}

impl Eq for HashKey {}

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

pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

#[derive(Debug, Clone)]
pub struct HashPair {
    pub key: Box<dyn Object>,
    pub value: Box<dyn Object>,
}

impl HashPair {
    pub fn new(key: Box<dyn Object>, value: Box<dyn Object>) -> Self {
        HashPair { key, value }
    }
}

#[derive(Debug, Clone)]
pub struct Hash {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl Hash {
    pub fn get(&self, key: &Box<dyn Object>) -> Option<&Box<dyn Object>> {
        if let Some(int) = key.as_any().downcast_ref::<Integer>() {
            return self.pairs.get(&int.hash_key()).map(|pair| &pair.value);
        } else if let Some(boolean) = key.as_any().downcast_ref::<Boolean>() {
            return self.pairs.get(&boolean.hash_key()).map(|pair| &pair.value);
        } else if let Some(string) = key.as_any().downcast_ref::<StringLit>() {
            return self.pairs.get(&string.hash_key()).map(|pair| &pair.value);
        }
        None
    }

    pub fn set(&mut self, key: Box<dyn Object>, value: Box<dyn Object>) -> Option<Box<dyn Object>> {
        // Try to cast the key to a hashable type
        if let Some(int) = key.as_any().downcast_ref::<Integer>() {
            let hash_key = int.hash_key();
            let pair = HashPair::new(key, value.clone());
            self.pairs.insert(hash_key, pair);
            Some(value)
        } else if let Some(boolean) = key.as_any().downcast_ref::<Boolean>() {
            let hash_key = boolean.hash_key();
            let pair = HashPair::new(key, value.clone());
            self.pairs.insert(hash_key, pair);
            Some(value)
        } else if let Some(string) = key.as_any().downcast_ref::<StringLit>() {
            let hash_key = string.hash_key();
            let pair = HashPair::new(key, value.clone());
            self.pairs.insert(hash_key, pair);
            Some(value)
        } else {
            None
        }
    }
}

impl Object for Hash {
    fn type_(&self) -> ObjectType {
        ObjectType::Hash
    }

    fn inspect(&self) -> String {
        let pairs: Vec<String> = self
            .pairs
            .values()
            .map(|pair| format!("{}: {}", pair.key.inspect(), pair.value.inspect()))
            .collect();
        format!("{{{}}}", pairs.join(", "))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
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

impl Hashable for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey {
            type_: self.type_(),
            value: self.value as u64,
        }
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

impl Hashable for Boolean {
    fn hash_key(&self) -> HashKey {
        HashKey {
            type_: self.type_(),
            value: if self.value { 1 } else { 0 },
        }
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

impl Hashable for StringLit {
    fn hash_key(&self) -> HashKey {
        let mut hasher = FnvHasher::default();
        hasher.write(self.value.as_bytes());
        HashKey {
            type_: self.type_(),
            value: hasher.finish(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringLit::new("Hello World".to_string());
        let hello2 = StringLit::new("Hello World".to_string());
        let diff1 = StringLit::new("My name is johnny".to_string());
        let diff2 = StringLit::new("My name is johnny".to_string());

        if hello1.hash_key() != hello2.hash_key() {
            panic!("strings with same content have different hash keys");
        }

        if diff1.hash_key() != diff2.hash_key() {
            panic!("strings with same content have different hash keys");
        }

        if hello1.hash_key() == diff1.hash_key() {
            panic!("strings with different content have same hash keys");
        }
    }
}
