use super::super::ast::*;
use super::ir::*;
use super::type_env::TypeInfo;
use super::variable_table::VariableTable;
use std::collections::HashMap;
use super::Error;

type AstToIrResult<T> = Result<T, Error>;

impl ProgramAST {
    //ASTをIRに変換
    pub fn to_ir(self) -> AstToIrResult<ProgramIr> {
        let mut lambda_count: usize = 0;
        let mut var_table =
            VariableTable::new(self.get_global_var_names());

        let program_ir = self.stmt_list
            .into_iter()
            .fold(
                Ok(ProgramIr::empty()),
                |acc, stmt|
                    stmt.to_ir(acc?, &mut lambda_count, &mut var_table),
            )?;
        Ok(program_ir)
    }
    //グローバル変数の名前一覧を取得（関数名しかないけど）
    fn get_global_var_names(&self) -> HashMap<String, ()> {
        self.stmt_list.iter().map(|x| {
            match x {
                StmtAST::DecFuncAST(x) => Some(x.name.clone()),
                StmtAST::DefFuncAST(x) => Some(x.name.clone()),
                _ => None
            }
        })
            .filter_map(|x| x)
            .map(|x| (x, ()))
            .collect()
    }
}


impl StmtAST {
    fn to_ir(self, program_ir: ProgramIr, lambda_count: &mut usize, var_table: &mut VariableTable)
             -> AstToIrResult<ProgramIr> {
        match self {
            StmtAST::DefFuncAST(x) => x.to_ir(program_ir, lambda_count, var_table),
            StmtAST::DecFuncAST(x) => Ok(x.to_ir(program_ir)),
            _ => Ok(program_ir),
        }
    }
}

impl DefFuncAST {
    fn to_ir(self, mut program_ir: ProgramIr, lambda_count: &mut usize, var_table: &mut VariableTable)
             -> AstToIrResult<ProgramIr> {
        let params_len: usize =
            if self.params.len() == 0 {
                1
            } else {
                self.params.len()
            };
        var_table.in_nest(self.params.into_iter().map(|x| x.id));
        let func_ir = FuncIr {
            name: self.name,
            body: self.body.to_ir(&mut program_ir, var_table, lambda_count)?,
            params_len,
            pos: self.pos,
        };
        program_ir.func_list.push(func_ir);
        var_table.out_nest();
        Ok(program_ir)
    }
}

impl DecFuncAST {
    fn to_ir(self, mut program_ir: ProgramIr) -> ProgramIr {
        match self.extern_flag {
            true => {
                program_ir.ex_dec_func_list.push(self);
            }
            _ => program_ir.dec_func_list.push(self)
        }
        program_ir
    }
}

impl ExprAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &mut VariableTable, lambda_count: &mut usize)
             -> AstToIrResult<ExprIr> {
        match self {
            ExprAST::NumAST(x) => Ok(ExprIr::NumIr(x)),
            ExprAST::OpAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::VariableAST(x) =>
                match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_info) {
                    Some(x) => Ok(x),
                    _ => Err(Error::new(x.pos, "not found variable"))
                },
            ExprAST::ParenAST(x) => x.expr.to_ir(program_ir, var_table, lambda_count),
            ExprAST::FuncCallAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::TupleAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::LambdaAST(x) => x.to_ir(program_ir, var_table, lambda_count)
        }
    }
}

impl OpAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &mut VariableTable, lambda_count: &mut usize)
             -> AstToIrResult<ExprIr> {
        Ok(
            ExprIr::create_opir(
                self.op,
                self.l_expr.to_ir(program_ir, var_table, lambda_count)?,
                self.r_expr.to_ir(program_ir, var_table, lambda_count)?,
            )
        )
    }
}

impl FuncCallAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &mut VariableTable, lambda_count: &mut usize)
             -> AstToIrResult<ExprIr> {
        let func = self.func.to_ir(program_ir, var_table, lambda_count);
        if self.params.len() == 0 {
            func
        } else {
            let params =
                self.params.into_iter().map(|x|
                    x.to_ir(program_ir, var_table, lambda_count)
                ).collect::<AstToIrResult<Vec<ExprIr>>>()?;
            Ok(ExprIr::create_callir(func?, params, program_ir.ty_info.no_name_get()))
        }
    }
}

impl TupleAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &mut VariableTable, lambda_count: &mut usize)
             -> AstToIrResult<ExprIr> {
        Ok(ExprIr::create_tupleir(
            self.elements.into_iter()
                .map(|x|
                    x.to_ir(program_ir, var_table, lambda_count)
                ).collect::<AstToIrResult<Vec<ExprIr>>>()?,
            self.pos,
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl LambdaAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &mut VariableTable, lambda_count: &mut usize)
             -> AstToIrResult<ExprIr> {
        let env =
            self.env.iter()
                .map(|x|
                    match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_info) {
                        Some(ExprIr::VariableIr(x)) => Ok(x),
                        _ =>  Err(Error::new(x.pos,"Lambda capture not Local Variable"))
                    }
                )
                .collect::<AstToIrResult<Vec<VariableIr>>>()?;
        let params_len: usize =
            if self.params.len() == 0 {
                1
            } else {
                self.params.len()
            };
        let params_len = params_len + self.env.len();
        let env_id_iter = self.env.iter().map(|x| x.id.clone());
        let params_id_iter = self.params.into_iter().map(|x| x.id);
        var_table.in_nest(&mut env_id_iter.chain(params_id_iter));
        let body = self.body.to_ir(program_ir, var_table, lambda_count)?;
        var_table.out_nest();
        *lambda_count += 1;
        let lambda_name = "#".to_string() + &(*lambda_count - 1).to_string();
        program_ir.func_list.push(FuncIr { params_len, body, pos: self.pos, name: lambda_name.clone() });
        Ok(ExprIr::LambdaIr(Box::new(LambdaIr {
            env,
            func_name: lambda_name,
            ty_id: program_ir.ty_info.no_name_get(),
            pos: self.pos,
            params_len,
        })))
    }
}

#[test]
fn ast_to_ir_test() {
    use combine::stream::state::SourcePosition;
    use compile::types::TypeId;
    let ast = ProgramAST {
        stmt_list: vec![StmtAST::DefFuncAST(DefFuncAST {
            name: "hoge".to_string(),
            params: vec![
                VariableAST::new("a".to_string(), SourcePosition { column: 0, line: 0 }),
                VariableAST::new("b".to_string(), SourcePosition { column: 0, line: 0 }),
            ],
            body: ExprAST::create_variable_ast(
                "b".to_string(),
                SourcePosition { column: 0, line: 0 },
            ),
            pos: SourcePosition { column: 0, line: 0 },
        })],
    };
    let mut func_list = vec![];
    func_list.push(
        FuncIr {
            name: "hoge".to_string(),
            body: ExprIr::create_variableir(0, SourcePosition { line: 0, column: 0 }, TypeId::new(0)),
            params_len: 2,
            pos: SourcePosition { column: 0, line: 0 },
        },
    );
    let ir = ProgramIr {
        dec_func_list: vec![],
        func_list,
        ex_dec_func_list: vec![],
        ty_info: TypeInfo::new()
    };
    assert_eq!(ast.to_ir().unwrap().0, ir);
}
