use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Box<dyn Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Box<dyn Object>> {
        self.store.get(name).cloned()
    }

    pub fn set(&mut self, name: &str, value: Box<dyn Object>) {
        self.store.insert(name.to_string(), value);
    }
}
