use crate::{Binding, BindingKind, ErrorReporting, ExprKind, ExprNode, StmtKind, StmtNode};

pub struct Codegen {
    pub source: Vec<u8>,
    pub nodes: Vec<StmtNode>,
    pub depth: i64,
    pub stack_size: usize,
    pub count: usize,
}

impl ErrorReporting for Codegen {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

impl Codegen {
    pub fn new(source: Vec<u8>, nodes: Vec<StmtNode>, stack_size: usize) -> Self {
        Self {
            source,
            nodes,
            stack_size,
            count: 0,
            depth: 0,
        }
    }

    pub fn program(&mut self) {
        println!("  .globl main");
        println!("main:");
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", self.stack_size);
        for stmt in self.nodes.clone() {
            self.stmt(&stmt);
        }
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

    fn addr(&mut self, node: &ExprNode) {
        match &node.kind {
            ExprKind::Var(Binding {
                kind: BindingKind::LocalVar { stack_offset },
                ..
            }) => {
                let loc = -(self.stack_size as i64) - ((stack_offset + 1) * 8);
                println!("  lea {}(%rbp), %rax", loc);
            }
            ExprKind::Deref(ref lhs) => self.expr(lhs),
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
            StmtKind::If(cond, then, r#else) => {
                let c = self.count();
                self.expr(cond);
                println!("  cmp $0, %rax");
                println!("  je  .L.else.{}", c);
                self.stmt(then);
                println!("  jmp .L.end.{}", c);
                println!(".L.else.{}:", c);
                if let Some(else_branch) = r#else {
                    self.stmt(else_branch);
                }
                println!(".L.end.{}:", c);
            }
            StmtKind::For(init, cond, incr, then) => {
                let c = self.count();
                if let Some(init) = init {
                    self.stmt(init);
                }
                println!(".L.begin.{}:", c);
                if let Some(cond) = cond {
                    self.expr(cond);
                    println!("  cmp $0, %rax");
                    println!("  je  .L.end.{}", c);
                }
                self.stmt(then);
                if let Some(incr) = incr {
                    self.expr(incr);
                }
                println!("  jmp .L.begin.{}", c);
                println!(".L.end.{}:", c);
            }
        }
    }

    fn count(&mut self) -> usize {
        self.count += 1;
        self.count
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
            ExprKind::Var(_) => {
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
            ExprKind::Addr(ref lhs) => {
                self.addr(lhs);
            }
            ExprKind::Deref(ref lhs) => {
                self.expr(lhs);
                println!("  mov (%rax), %rax");
            }
            ExprKind::Funcall(ref name) => {
                println!("  mov $0, %rax");
                println!("  call {}", String::from_utf8_lossy(name));
            }
        };
    }
}
