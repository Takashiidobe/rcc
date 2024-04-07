use crate::{ErrorReporting, ExprKind, ExprNode, StmtNode};

pub struct Codegen {
    pub source: Vec<u8>,
    pub nodes: Vec<StmtNode>,
    pub depth: i64,
}

impl ErrorReporting for Codegen {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

impl Codegen {
    pub fn new(source: Vec<u8>, nodes: Vec<StmtNode>) -> Self {
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
            self.stmt(&node);
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

    fn stmt(&mut self, node: &StmtNode) {
        match &node.kind {
            crate::StmtKind::Expr(lhs) => self.expr(lhs),
            _ => panic!("invalid statement"),
        }
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
            ExprKind::Neg(ref rhs) => {
                self.expr(rhs);
                println!("  neg %rax");
            }
            ExprKind::Eq(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  sete %al");
                println!("  movzb %al, %rax");
            }
            ExprKind::Ne(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setne %al");
                println!("  movzb %al, %rax");
            }
            ExprKind::Lte(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setle %al");
                println!("  movzb %al, %rax");
            }
            ExprKind::Lt(ref lhs, ref rhs) => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setl %al");
                println!("  movzb %al, %rax");
            }
        };
    }
}
