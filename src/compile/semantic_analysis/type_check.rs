use std::collections::HashMap;
use super::super::types::*;
use super::ir::*;
use super::super::error::Error;

type TyCheckResult<T> = Result<T, Error>;

impl ProgramIr {
    pub fn ty_check(mut self) -> TyCheckResult<ProgramIr> {
        let func_name_list: Vec<String> =
            self.func_list.keys()
                .map(|x|x.clone())
                .clone().collect();
        for func_name in func_name_list {
            let func_ir = self.func_list.remove(&func_name).unwrap()
                .ty_check(&mut self.func_list, &self.ex_func_list, Type::Unknown)?;
            self.func_list.insert(func_name, func_ir);
        }
        Ok(self)
    }
}

type FuncList = HashMap<String, FuncIr>;
type ExFuncList = HashMap<String, DecFuncIr>;

impl DecFuncIr {
    fn ty_check(&self, expect_ty: Type) -> TyCheckResult<FuncType> {
        match expect_ty {
            Type::Unknown => Ok(self.ty.clone()),
            Type::Fn(x) =>
                if self.ty >= *x {
                    Ok(self.ty.clone())
                } else {
                    panic!("type error!")
                }
            _ => panic!("type error!")
        }
    }
}

impl FuncIr {
    fn ty_check(mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, expect_ty: Type) -> TyCheckResult<FuncIr> {
        match match expect_ty {
            Type::Fn(x) => self.ty.merge(Type::FuncType(Box::new(*x))),
            x => self.ty.merge(x)
        }
            {
                Ok(x) => self.ty = x,
                Err(msg) => return Err(Error::new(self.pos, &msg))
            };
        self.body =
            self.body.ty_check(func_list, ex_func_list, &mut self.ty.param_types, self.ty.ret_type)?;
        self.ty.ret_type = self.body.get_ty().clone();
        Ok(self)
    }
}


impl ExprIr {
    fn ty_check(
        self,
        func_list: &mut HashMap<String, FuncIr>,
        ex_func_list: &ExFuncList,
        params: &mut Vec<Type>,
        expect_ty: Type,
    ) -> TyCheckResult<ExprIr> {
        Ok(match self {
            ExprIr::NumIr(x) =>
                ExprIr::NumIr(x.ty_check()),
            ExprIr::CallIr(x) =>
                ExprIr::CallIr(Box::new(x.ty_check(func_list, ex_func_list, params, expect_ty)?)),
            ExprIr::OpIr(x) =>
                ExprIr::OpIr(Box::new(x.ty_check(func_list, ex_func_list, params)?)),
            ExprIr::VariableIr(x) =>
                ExprIr::VariableIr(x.ty_check(params, expect_ty)?),
            ExprIr::GlobalVariableIr(x) =>
                ExprIr::GlobalVariableIr(x.ty_check(func_list, ex_func_list, expect_ty)?),
        })
    }
}

impl NumIr {
    fn ty_check(mut self) -> NumIr {
        if self.ty == Type::Unknown {
            self.ty = Type::Int32;
        };
        self
    }
}

impl CallIr {
    fn ty_check(
        mut self,
        func_list: &mut FuncList,
        ex_func_list: &ExFuncList,
        params: &mut Vec<Type>,
        expect_ty: Type,
    ) -> TyCheckResult<CallIr> {
        self.params =
            self.params.into_iter().map(|x| {
                x.ty_check(func_list, ex_func_list, params, Type::Unknown)
            }).collect::<TyCheckResult<Vec<ExprIr>>>()?;
        let expect_func_ty = Type::create_fn_func_type(
            self.params.iter().map(|x| x.get_ty().clone()).collect(),
            expect_ty.clone(),
        );

        self.func = self.func.ty_check(func_list, ex_func_list, params, expect_func_ty)?;

        match self.func.get_ty() {
            Type::Fn(x) => self.ty = x.ret_type.clone(),
            _ => panic!("type error!")
        };

        Ok(self)
    }
}

impl OpIr {
    fn ty_check(mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, params: &mut Vec<Type>) -> TyCheckResult<OpIr> {
        if self.ty != Type::Unknown {
            return Ok(self);
        }

        self.l_expr = self.l_expr.ty_check(func_list, ex_func_list, params, Type::Int32)?;
        self.r_expr = self.r_expr.ty_check(func_list, ex_func_list, params, Type::Int32)?;
        self.ty = Type::Int32;
        Ok(self)
    }
}

impl VariableIr {
    fn ty_check(mut self, params: &mut Vec<Type>, expect_ty: Type) -> TyCheckResult<VariableIr> {
        let mut param = params[params.len() - self.id - 1].clone();
        match param.merge(expect_ty) {
            Ok(ty) => param = ty,
            Err(msg) => return Err(Error::new(self.pos, &msg))
        }
        let len = params.len();
        params[len - self.id - 1] = param.clone();
        self.ty = param;
        Ok(self)
    }
}

impl GlobalVariableIr {
    fn ty_check(mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, expect_ty: Type) -> TyCheckResult<GlobalVariableIr> {
        let func_ty =
            match func_list.remove(&self.id) {
                Some(x) => {
                    let func_ir = x.ty_check(func_list, ex_func_list, expect_ty)?;
                    let ty = func_ir.ty.clone();
                    func_list.insert(self.id.clone(), func_ir);
                    Ok(ty)
                }
                _ =>
                    match ex_func_list.get(&self.id) {
                        Some(x) => x.ty_check(expect_ty),
                        _ => return Err(Error::new(self.pos, "call not found function"))
                    }
            };
        self.ty = Type::Fn(Box::new(func_ty?));
        Ok(self)
    }
}