use std::cell::RefCell;

thread_local! {
    static TRACE_LEVEL: RefCell<usize> = const { RefCell::new(0) };
}

fn ident_level() -> String {
    TRACE_LEVEL.with(|level| {
        let count = *level.borrow();
        if count > 0 {
            "\t".repeat(count - 1)
        } else {
            String::new()
        }
    })
}

pub struct TraceGuard {
    msg: String,
}

impl TraceGuard {
    pub fn new(msg: &str) -> Self {
        TRACE_LEVEL.with(|level| {
            *level.borrow_mut() += 1;
            println!("{}BEGIN {}", ident_level(), msg);
        });
        Self {
            msg: msg.to_string(),
        }
    }
}

impl Drop for TraceGuard {
    fn drop(&mut self) {
        TRACE_LEVEL.with(|level| {
            println!("{}END {}", ident_level(), self.msg);
            *level.borrow_mut() -= 1;
        });
    }
}
