use crate::{Binding, BindingKind, ErrorReporting, ExprKind, ExprNode, StmtKind, StmtNode};

pub struct Codegen {
    pub source: Vec<u8>,
    pub node: StmtNode,
    pub depth: i64,
    pub stack_size: usize,
}

impl ErrorReporting for Codegen {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

impl Codegen {
    pub fn new(source: Vec<u8>, node: StmtNode, stack_size: usize) -> Self {
        Self {
            source,
            node,
            depth: 0,
            stack_size,
        }
    }

    pub fn program(&mut self) {
        println!("  .globl main");
        println!("main:");
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", self.stack_size);
        self.stmt(&self.node.clone());
        println!(".L.return:");
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
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

    fn addr(&self, node: &Binding) {
        match &node.kind {
            BindingKind::LocalVar { stack_offset } => {
                println!("  lea {}(%rbp), %rax", stack_offset);
            }
            _ => self.error_at(node.loc.offset, "not an lvalue"),
        }
    }

    fn stmt(&mut self, node: &StmtNode) {
        match &node.kind {
            StmtKind::Expr(lhs) => self.expr(lhs),
            StmtKind::Return(lhs) => {
                self.expr(lhs);
                println!("  jmp .L.return");
            }
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.stmt(stmt);
                }
            }
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
            ExprKind::Var(ref node) => {
                self.addr(node);
                println!("  mov (%rax), %rax");
            }
            ExprKind::Assign(ref lhs, ref rhs) => {
                self.addr(lhs);
                self.push();
                self.expr(rhs);
                self.pop("%rdi");
                println!("  mov %rax, (%rdi)");
            }
        };
    }
}
