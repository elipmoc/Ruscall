use super::super::ir::ast::*;
use super::super::ir::hir::*;
use super::Error;
use std::collections::HashMap;

type AstToHirResult<T> = Result<T, Error>;

impl ProgramAST {
    //ASTをHIRに変換
    pub fn to_hir(self) -> AstToHirResult<ProgramHir> {
        let mut program_hir = ProgramHir {
            infix_list: HashMap::new(),
            dec_func_list: HashMap::new(),
            def_func_list: vec![],
            ex_dec_func_list: HashMap::new(),
        };
        self
            .stmt_list
            .into_iter()
            .for_each(|stmt| {
                match stmt {
                    StmtAST::InfixAST(x) => { program_hir.infix_list.insert(x.op.clone(), x); }
                    StmtAST::DefFuncAST(x) => program_hir.def_func_list.push(x),
                    StmtAST::DecFuncAST(x) => {
                        if x.extern_flag {
                            program_hir.ex_dec_func_list.insert(x.name.clone(),x);
                        } else {
                            program_hir.dec_func_list.insert(x.name.clone(),x);
                        }
                    }
                    StmtAST::NoneAST => ()
                };
            });
        Ok(program_hir)
    }
}