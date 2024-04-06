use crate::{ErrorReporting, ExprKind, ExprNode};

pub struct Codegen {
    pub source: Vec<u8>,
    pub nodes: Vec<ExprNode>,
    pub depth: i64,
}

impl ErrorReporting for Codegen {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

impl Codegen {
    pub fn new(source: Vec<u8>, nodes: Vec<ExprNode>) -> Self {
        Self {
            source,
            nodes,
            depth: 0,
        }
    }

    pub fn program(&mut self) {
        println!("  .globl main");
        println!("main:");
        for node in self.nodes.clone() {
            self.expr(&node);
        }
        println!("  ret");
    }

    fn push(&mut self) {
        println!("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, arg: &str) {
        println!("  pop {}", arg);
        self.depth -= 1;
    }

    fn expr(&mut self, node: &ExprNode) {
        match node.kind {
            ExprKind::Number(val) => println!("  mov ${}, %rax", val),
            ExprKind::Add(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  add %rdi, %rax");
            }
            ExprKind::Sub(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  sub %rdi, %rax");
            }
            ExprKind::Mul(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  imul %rdi, %rax");
            }
            ExprKind::Div(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cqo");
                println!("  idiv %rdi, %rax");
            }
        };
    }
}
