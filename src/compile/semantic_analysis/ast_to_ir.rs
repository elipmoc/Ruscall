use super::super::ir::ast::*;
use super::mir::*;
use super::variable_table::VariableTable;
use super::super::types::types::*;
use super::type_env::TypeInfo;
use super::Error;
use super::type_variable_table::TypeVariableTable;
use std::collections::HashMap;

type AstToIrResult<T> = Result<T, Error>;

impl ProgramAST {
    //ASTをIRに変換
    pub fn to_ir(self) -> AstToIrResult<ProgramMir> {
        let mut lambda_count: usize = 0;
        let mut var_table = VariableTable::new(self.get_global_var_names());

        let program_ir = self
            .stmt_list
            .into_iter()
            .fold(Ok(ProgramMir::empty()), |acc, stmt| {
                stmt.to_ir(acc?, &mut lambda_count, &mut var_table)
            })?;
        Ok(program_ir)
    }
    //グローバル変数の名前一覧を取得（関数名しかないけど）
    fn get_global_var_names(&self) -> HashMap<String, ()> {
        self.stmt_list
            .iter()
            .map(|x| match x {
                StmtAST::DecFuncAST(x) => Some(x.name.clone()),
                StmtAST::DefFuncAST(x) => Some(x.name.clone()),
                _ => None,
            }).filter_map(|x| x)
            .map(|x| (x, ()))
            .collect()
    }
}

impl StmtAST {
    fn to_ir(
        self,
        program_ir: ProgramMir,
        lambda_count: &mut usize,
        var_table: &mut VariableTable,
    ) -> AstToIrResult<ProgramMir> {
        match self {
            StmtAST::DefFuncAST(x) => x.to_ir(program_ir, lambda_count, var_table),
            StmtAST::DecFuncAST(x) => Ok(x.to_ir(program_ir)),
            _ => Ok(program_ir),
        }
    }
}

impl DefFuncAST {
    fn to_ir(
        self,
        mut program_ir: ProgramMir,
        lambda_count: &mut usize,
        var_table: &mut VariableTable,
    ) -> AstToIrResult<ProgramMir> {
        let params_len: usize = if self.params.len() == 0 {
            1
        } else {
            self.params.len()
        };
        var_table.in_nest(self.params.into_iter().map(|x| x.id));
        let func_ir = FuncMir {
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
    fn to_ir(self, mut program_ir: ProgramMir) -> ProgramMir {
        let mut ty_var_table = TypeVariableTable::new();
        let dec_func_ir = DecFuncMir {
            name: self.name,
            extern_flag: self.extern_flag,
            pos: self.pos,
            ty: self.ty.to_ty(&mut ty_var_table, &mut program_ir.ty_info),
        };
        match self.extern_flag {
            true => {
                program_ir.ex_dec_func_list.push(dec_func_ir);
            }
            _ => program_ir.dec_func_list.push(dec_func_ir),
        }
        program_ir
    }
}

impl FuncTypeAST {
    fn to_ty(self, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> FuncType {
        FuncType {
            ret_type: self.ret_ty.to_ty(ty_var_table, ty_info),
            param_types: self.params_ty.into_iter()
                .map(|x| x.to_ty(ty_var_table, ty_info)).collect(),
        }
    }
}

impl TupleTypeAST {
    fn to_ty(self, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> TupleType {
        TupleType {
            element_tys: self.elements_ty.into_iter()
                .map(|x| x.to_ty(ty_var_table, ty_info)).collect()
        }
    }
}

impl TypeAST {
    fn to_ty(self, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> Type {
        match self {
            TypeAST::Type(x) => x,
            TypeAST::FuncTypeAST(x) =>Type::TyVar(ty_info.no_name_get(),vec![TypeCondition::Call(x.to_ty(ty_var_table,ty_info))]),
            TypeAST::TupleTypeAST(x) => Type::TupleType(
                Box::new(x.to_ty(ty_var_table, ty_info))
            ),
            TypeAST::TypeVarName(ty_name) => ty_var_table.get_ty(ty_name, ty_info)
        }
    }
}

impl ExprAST {
    fn to_ir(
        self,
        program_ir: &mut ProgramMir,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        match self {
            ExprAST::NumAST(x) => Ok(ExprMir::NumMir(x)),
            ExprAST::BoolAST(x)=> Ok(ExprMir::BoolMir(x)),
            ExprAST::OpAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::VariableAST(x) => {
                match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_info) {
                    Some(x) => Ok(x),
                    _ => Err(Error::new(x.pos, "not found variable")),
                }
            }
            ExprAST::ParenAST(x) => x.expr.to_ir(program_ir, var_table, lambda_count),
            ExprAST::FuncCallAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::TupleAST(x) => x.to_ir(program_ir, var_table, lambda_count),
            ExprAST::LambdaAST(x) => x.to_ir(program_ir, var_table, lambda_count),
        }
    }
}

impl OpAST {
    fn to_ir(
        self,
        program_ir: &mut ProgramMir,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_op_mir(
            self.op,
            self.l_expr.to_ir(program_ir, var_table, lambda_count)?,
            self.r_expr.to_ir(program_ir, var_table, lambda_count)?,
        ))
    }
}

impl FuncCallAST {
    fn to_ir(
        self,
        program_ir: &mut ProgramMir,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let func = self.func.to_ir(program_ir, var_table, lambda_count);
        if self.params.len() == 0 {
            func
        } else {
            let params = self
                .params
                .into_iter()
                .map(|x| x.to_ir(program_ir, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?;
            Ok(ExprMir::create_call_mir(
                func?,
                params,
                program_ir.ty_info.no_name_get(),
            ))
        }
    }
}

impl TupleAST {
    fn to_ir(
        self,
        program_ir: &mut ProgramMir,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_tuple_mir(
            self.elements
                .into_iter()
                .map(|x| x.to_ir(program_ir, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?,
            self.pos,
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl LambdaAST {
    fn to_ir(
        self,
        program_ir: &mut ProgramMir,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let env = self
            .env
            .iter()
            .map(
                |x| match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_info) {
                    Some(ExprMir::VariableMir(x)) => Ok(x),
                    _ => Err(Error::new(x.pos, "Lambda capture not Local Variable")),
                },
            ).collect::<AstToIrResult<Vec<VariableMir>>>()?;
        let params_len: usize = if self.params.len() == 0 {
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
        program_ir.func_list.push(FuncMir {
            params_len,
            body,
            pos: self.pos,
            name: lambda_name.clone(),
        });
        Ok(ExprMir::LambdaMir(Box::new(LambdaMir {
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
    use super::type_env::TypeInfo;
    use combine::stream::state::SourcePosition;
    use compile::types::types::TypeId;
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
    func_list.push(FuncMir {
        name: "hoge".to_string(),
        body: ExprMir::create_variable_mir(0, SourcePosition { line: 0, column: 0 }, TypeId::new(0)),
        params_len: 2,
        pos: SourcePosition { column: 0, line: 0 },
    });
    let ir = ProgramMir {
        dec_func_list: vec![],
        func_list,
        ex_dec_func_list: vec![],
        ty_info: TypeInfo::new(),
    };
    let ir2 = ast.to_ir().unwrap();
    assert_eq!(ir2.func_list, ir.func_list);
    assert_eq!(ir2.dec_func_list, ir.dec_func_list);
    assert_eq!(ir2.ex_dec_func_list, ir.ex_dec_func_list);
}
