use crate::ast::{Expr, ExprBlock, Program, Span, Stmt};
use crate::parse;
use clean_path::Clean;
use itertools::Itertools;
use miette::Diagnostic;
use petgraph::algo::{tarjan_scc, toposort};
use petgraph::graph::NodeIndex;
use petgraph::{Directed, Graph};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use thiserror::Error;

pub struct Resolver {
    stack: Vec<NodeIndex>,
    deps: Vec<PathBuf>,
    file_nodes: HashMap<PathBuf, NodeIndex>,
    visited_files: HashSet<PathBuf>,
    file_graph: Graph<PathBuf, (), Directed>,
    diagnostics: Vec<crate::diagnostics::Diagnostic>,
}

#[derive(Debug, Error, Diagnostic)]
pub enum ResolverDiagnostic {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    // TODO: Add code span if exists
    #[error("file {path} does not exist")]
    FileDoesNotExist { path: PathBuf },
    #[error("cycle detected in dependency graph: {cycles}")]
    CycleDetected { cycles: String },
}

impl Resolver {
    pub fn new(root_file: PathBuf) -> Self {
        let mut file_graph = Graph::new();
        let root_idx = file_graph.add_node(root_file.clone());
        let mut file_nodes = HashMap::new();
        file_nodes.insert(root_file, root_idx);
        Self {
            stack: vec![root_idx],
            deps: Vec::new(),
            file_nodes,
            visited_files: HashSet::new(),
            file_graph,
            diagnostics: Vec::new(),
        }
    }

    pub fn build(&mut self) {
        while let Some(file_idx) = self.stack.pop() {
            let file = self.file_graph[file_idx].clone();
            if self.visited_files.contains(&file) {
                continue;
            }
            self.add_deps(&file);

            for dep in &self.deps {
                let dep_idx = self
                    .file_nodes
                    .entry(dep.clone())
                    .or_insert_with(|| self.file_graph.add_node(dep.clone()))
                    .clone();

                self.file_graph.add_edge(dep_idx, file_idx, ());
                self.stack.push(dep_idx);
            }
            self.deps.clear();
            self.visited_files.insert(file.clone());
        }
    }
    pub fn traverse(&mut self) {
        match toposort(&self.file_graph, None) {
            Ok(nodes) => {
                nodes.iter().for_each(|node_idx| {
                    println!("{}", self.file_graph[*node_idx].display());
                });
            }
            Err(_) => {
                // If only the cycle error returned good information...
                let cycles = tarjan_scc(&self.file_graph);
                let cycles = cycles
                    .into_iter()
                    .map(|cycle| {
                        if cycle.len() == 2 {
                            cycle
                                .into_iter()
                                .map(|node| self.file_graph[node].display())
                                .join(" <-> ")
                        } else {
                            cycle
                                .into_iter()
                                .map(|node| self.file_graph[node].display())
                                .join(" -> ")
                        }
                    })
                    .join("\n");

                self.diagnostics
                    .push(crate::diagnostics::Diagnostic::Resolver(
                        ResolverDiagnostic::CycleDetected { cycles },
                    ))
            }
        }
    }

    pub fn into_diagnostics(self) -> Vec<crate::diagnostics::Diagnostic> {
        self.diagnostics
    }

    pub fn print_graph(&self) {
        println!("{:?}", petgraph::dot::Dot::new(&self.file_graph));
    }

    fn add_deps(&mut self, file: &Path) {
        let Ok(file_contents) = std::fs::read_to_string(&file) else {
            self.diagnostics
                .push(crate::diagnostics::Diagnostic::Resolver(
                    ResolverDiagnostic::FileDoesNotExist {
                        path: file.to_path_buf(),
                    },
                ));
            return;
        };
        let (program, diagnostics) = parse(&file_contents);
        self.diagnostics.extend(
            diagnostics
                .into_iter()
                .map(crate::diagnostics::Diagnostic::Parse),
        );
        let Some(program) = program else { return };
        self.get_imports_in_program(&file, &program);
    }

    fn get_imports_in_program(&mut self, current_path: &Path, program: &Program) {
        for stmt in &program.statements {
            self.get_imports_in_stmt(current_path, stmt);
        }
    }

    fn get_imports_in_stmt(&mut self, current_path: &Path, stmt: &Span<Stmt>) {
        match &stmt.0 {
            Stmt::Import { path, .. } => {
                let mut import_path = current_path.parent().unwrap().join(&path.0);
                import_path.set_extension("vc");
                self.deps.push(import_path.clean());
            }
            Stmt::Let(_, expr) => self.get_imports_in_expr(current_path, expr),
            Stmt::Expr(expr) => {
                self.get_imports_in_expr(current_path, expr);
            }
            Stmt::Function(function) => {
                self.get_imports_in_expr_block(current_path, &function.body);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.get_imports_in_expr(current_path, condition);
                for stmt in then_block {
                    self.get_imports_in_stmt(current_path, stmt);
                }
                for stmt in else_block {
                    self.get_imports_in_stmt(current_path, stmt);
                }
            }
            Stmt::For { iterator, body, .. } => {
                self.get_imports_in_expr(current_path, iterator);
                for stmt in body {
                    self.get_imports_in_stmt(current_path, stmt);
                }
            }
            Stmt::Return(Some(expr)) => {
                self.get_imports_in_expr(current_path, expr);
            }
            Stmt::Export { .. } => {}
            Stmt::Type(_) => {}
            Stmt::Use { .. } => {}
            Stmt::Return(None) => {}
        }
    }

    fn get_imports_in_expr(&mut self, current_path: &Path, expr: &Span<Expr>) {
        match &expr.0 {
            Expr::PostFix(expr, _) => {
                self.get_imports_in_expr(current_path, expr);
            }
            Expr::Binary(_, lhs, rhs) => {
                self.get_imports_in_expr(current_path, lhs);
                self.get_imports_in_expr(current_path, rhs);
            }
            Expr::Unary(_, rhs) => {
                self.get_imports_in_expr(current_path, rhs);
            }
            Expr::Struct(_, fields) => {
                fields.for_each(|expr| self.get_imports_in_expr(current_path, expr));
            }
            Expr::Enum { fields, .. } => {
                fields.for_each(|expr| self.get_imports_in_expr(current_path, expr));
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.get_imports_in_expr(current_path, condition);
                self.get_imports_in_expr_block(current_path, then_block);
                self.get_imports_in_expr_block(current_path, else_block);
            }
            Expr::Match { expr, cases } => {
                self.get_imports_in_expr(current_path, expr);
                for (_, block) in cases {
                    self.get_imports_in_expr_block(current_path, block);
                }
            }
            Expr::Array(elements) => {
                for elem in elements {
                    self.get_imports_in_expr(current_path, elem);
                }
            }
            Expr::Value(_) => {}
            Expr::Variable(_) => {}
        }
    }

    fn get_imports_in_expr_block(&mut self, current_path: &Path, expr_block: &Span<ExprBlock>) {
        for stmt in &expr_block.0.stmts {
            self.get_imports_in_stmt(current_path, stmt);
        }
        if let Some(end_expr) = &expr_block.0.end_expr {
            self.get_imports_in_expr(current_path, end_expr);
        }
    }
}
