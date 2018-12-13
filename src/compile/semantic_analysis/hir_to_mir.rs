use super::super::ir::ast::*;
use super::mir::*;
use super::super::ir::hir::*;
use super::variable_table::VariableTable;
use super::super::types::types::*;
use super::type_env::TypeInfo;
use super::Error;
use super::type_variable_table::TypeVariableTable;
use std::collections::HashMap;

type AstToIrResult<T> = Result<T, Error>;

impl ProgramHir {
    //ASTをIRに変換
    pub fn to_mir(self) -> AstToIrResult<ProgramMir> {
        let mut lambda_count: usize = 0;
        let mut var_table = VariableTable::new(self.get_global_var_names());

        let mut program_mir = ProgramMir::empty();
        let struct_list = &self.struct_list;
        program_mir = self.def_func_list
            .into_iter()
            .fold(Ok(program_mir), |acc, (_, x)| {
                x.to_mir(acc?, struct_list, &mut lambda_count, &mut var_table)
            })?;

        program_mir = self.dec_func_list
            .into_iter()
            .chain(self.ex_dec_func_list.into_iter())
            .fold(Ok(program_mir), |acc, (_, x)| {
                Ok(x.to_mir(acc?, struct_list))
            })?;
        Ok(program_mir)
    }
    //グローバル変数の名前一覧を取得（関数名しかないけど）
    fn get_global_var_names(&self) -> HashMap<String, ()> {
        self.def_func_list.iter()
            .map(|(_, x)| x.name.clone())
            .chain(
                self.dec_func_list.iter()
                    .chain(self.ex_dec_func_list.iter())
                    .map(|(_, x)| x.name.clone())
            )
            .map(|x| (x, ()))
            .collect()
    }
}


impl DefFuncAST {
    fn to_mir(
        self,
        mut program_ir: ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
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
            body: self.body.to_mir(&mut program_ir, struct_list, var_table, lambda_count)?,
            params_len,
            pos: self.pos,
        };
        program_ir.func_list.push(func_ir);
        var_table.out_nest();
        Ok(program_ir)
    }
}

impl DecFuncAST {
    fn to_mir(self, mut program_ir: ProgramMir, struct_list: &HashMap<String, DecStructHir>) -> ProgramMir {
        let mut ty_var_table = TypeVariableTable::new();
        let dec_func_ir = DecFuncMir {
            name: self.name,
            extern_flag: self.extern_flag,
            pos: self.pos,
            ty: self.ty.to_ty(struct_list, &mut ty_var_table, &mut program_ir.ty_info),
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
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> FuncType {
        FuncType {
            ret_type: self.ret_ty.to_ty(struct_list, ty_var_table, ty_info),
            param_types: self.params_ty.into_iter()
                .map(|x| x.to_ty(struct_list, ty_var_table, ty_info)).collect(),
        }
    }
}

impl TupleTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> TupleType {
        TupleType {
            element_tys: self.elements_ty.into_iter()
                .map(|x| x.to_ty(struct_list, ty_var_table, ty_info)).collect()
        }
    }
}

impl RecordTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> RecordType {
        RecordType {
            element_tys: self.elements_ty.into_iter()
                .map(|(name, type_ast)|
                    (name, type_ast.to_ty(struct_list, ty_var_table, ty_info))
                ).collect()
        }
    }
}

impl StructTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> StructType {
        let internal_ty =
            match self.ty {
                StructInternalTypeAST::TupleTypeAST(x) => StructInternalType::TupleType(x.to_ty(struct_list, ty_var_table, ty_info)),
                StructInternalTypeAST::RecordTypeAST(x) => StructInternalType::RecordType(x.to_ty(struct_list, ty_var_table, ty_info))
            };
        StructType { ty: internal_ty, name: self.name }
    }
}

impl TypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_info: &mut TypeInfo) -> Type {
        match self {
            TypeAST::Type(x) => x,
            TypeAST::FuncTypeAST(x) => Type::TyVar(ty_info.no_name_get(), vec![TypeCondition::Call(x.to_ty(struct_list, ty_var_table, ty_info))]),
            TypeAST::TupleTypeAST(x) => Type::TupleType(
                Box::new(x.to_ty(struct_list, ty_var_table, ty_info))
            ),
            TypeAST::TypeVarName(ty_name) => ty_var_table.get_ty(ty_name, ty_info),
            TypeAST::StructTypeAST(x) => Type::StructType(Box::new(x.to_ty(struct_list, ty_var_table, ty_info))),
            TypeAST::IdTypeAST(id) => {
                if struct_list.contains_key(&id) {
                    Type::StructType(Box::new((&struct_list[&id]).ty.clone().to_ty(struct_list, ty_var_table, ty_info)))
                } else {
                    panic!("実装めんどいな")
                }
            }
            _ => panic!("undefined")
        }
    }
}

impl ExprAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        match self {
            ExprAST::NumAST(x) => Ok(ExprMir::NumMir(x)),
            ExprAST::BoolAST(x) => Ok(ExprMir::BoolMir(x)),
            ExprAST::IfAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::OpAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::VariableAST(x) => {
                match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_info) {
                    Some(x) => Ok(x),
                    _ => Err(Error::new(x.pos, "not found variable")),
                }
            }
            ExprAST::ParenAST(x) => x.expr.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::FuncCallAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::NamedParamsConstructorCallAST(_) => panic!("bug!!"),
            ExprAST::TupleAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::TupleStructAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::LambdaAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            _ => panic!("undefined")
        }
    }
}

impl IfAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_if_mir(
            self.cond.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.t_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.f_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.pos,
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl OpAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_op_mir(
            self.op,
            self.l_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.r_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
        ))
    }
}

impl FuncCallAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let func = self.func.to_mir(program_ir, struct_list, var_table, lambda_count);

        let param = self
            .param.to_mir(program_ir, struct_list, var_table, lambda_count)?;
        Ok(ExprMir::create_call_mir(
            func?,
            vec![param],
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl TupleAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_tuple_mir(
            self.elements
                .into_iter()
                .map(|x| x.to_mir(program_ir, struct_list, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?,
            self.pos,
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl TupleStructAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let mut ty_var_table = TypeVariableTable::new();
        Ok(ExprMir::create_tuple_struct_mir(
            self.tuple.elements
                .into_iter()
                .map(|x| x.to_mir(program_ir, struct_list, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?,
            self.tuple.pos,
            self.ty.to_ty(struct_list, &mut ty_var_table, &mut program_ir.ty_info),
            program_ir.ty_info.no_name_get(),
        ))
    }
}

impl LambdaAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
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
        let body = self.body.to_mir(program_ir, struct_list, var_table, lambda_count)?;
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
            name: "main".to_string(),
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
        name: "main".to_string(),
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
    let ir2 = ast.to_hir().unwrap().to_mir().unwrap();
    assert_eq!(ir2.func_list, ir.func_list);
    assert_eq!(ir2.dec_func_list, ir.dec_func_list);
    assert_eq!(ir2.ex_dec_func_list, ir.ex_dec_func_list);
}
