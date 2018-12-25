use super::super::ir::ast::*;
use super::super::ir::hir::*;
use super::Error;
use std::collections::HashMap;
use combine::stream::state::SourcePosition;

type AstToHirResult<T> = Result<T, Error>;

impl ProgramAST {
    //ASTをHIRに変換
    pub fn to_hir(self) -> AstToHirResult<ProgramHir> {
        let mut program_hir = ProgramHir {
            infix_list: HashMap::new(),
            dec_func_list: HashMap::new(),
            def_func_list: HashMap::new(),
            ex_dec_func_list: HashMap::new(),
            struct_list: HashMap::new(),
        };
        for stmt in self.stmt_list.into_iter() {
            match stmt {
                StmtAST::InfixAST(x) => { program_hir.infix_list.insert(x.op.clone(), x); }
                StmtAST::DefFuncAST(x) => {
                    if program_hir.def_func_list.contains_key(&x.name) {
                        return Err(Error::new(x.pos, &"Duplicate function"));
                    }
                    program_hir.def_func_list.insert(x.name.clone(), x);
                }
                StmtAST::DecFuncAST(x) => {
                    if x.extern_flag {
                        if program_hir.ex_dec_func_list.contains_key(&x.name) {
                            return Err(Error::new(x.pos, &"Duplicate function declare"));
                        }
                        program_hir.ex_dec_func_list.insert(x.name.clone(), x);
                    } else {
                        if program_hir.dec_func_list.contains_key(&x.name) {
                            return Err(Error::new(x.pos, "Duplicate function declare"));
                        }
                        program_hir.dec_func_list.insert(x.name.clone(), x);
                    }
                }
                StmtAST::NoneAST => (),
                StmtAST::DecStructAST(x) => {
                    if program_hir.struct_list.contains_key(&x.ty.name) {
                        return Err(Error::new(x.pos, &"Duplicate struct declare"));
                    }
                    program_hir.struct_list.insert(x.ty.name.clone(), x);
                }
            };
        };
        if program_hir.def_func_list.contains_key("main") {
            Ok(program_hir)
        } else {
            Err(Error::new(SourcePosition::new(), "not found main function!"))
        }
    }
}