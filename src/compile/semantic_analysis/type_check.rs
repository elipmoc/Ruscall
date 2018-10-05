use std::collections::HashMap;
use super::super::types::*;
use super::ir::*;

impl ProgramIr {
    pub fn ty_check(mut self) -> ProgramIr {
        let func_name_list: Vec<String> =
            self.func_list.values().clone()
                .into_iter().map(|funcir| {
                funcir.name.clone()
            }).collect();
        for func_name in func_name_list {
            let func_ir = self.func_list.remove(&func_name).unwrap();
            func_ir.ty_check(&mut self.func_list, &self.ex_func_list, Type::Unknown);
        }
        self
    }
}

type FuncList = HashMap<String, FuncIr>;
type ExFuncList = HashMap<String, DecFuncIr>;

impl DecFuncIr {
    fn ty_check(&self, expect_ty: Type) -> FuncType {
        match expect_ty {
            Type::Unknown => self.ty.clone(),
            Type::Fn(x) =>
                if self.ty >= *x {
                    self.ty.clone()
                } else {
                    panic!("type error!")
                }
            _ => panic!("type error!")
        }
    }
}

impl FuncIr {
    fn ty_check(mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, expect_ty: Type) -> FuncType {
        match match expect_ty {
            Type::Fn(x) => self.ty.merge(Type::FuncType(Box::new(*x))),
            x => self.ty.merge(x)
        }
            {
                Some(x) => self.ty = x,
                _ => panic!("type error!")
            };
        self.body.ty_check(func_list, ex_func_list, &mut self.ty.param_types, self.ty.ret_type);
        self.ty.ret_type = self.body.get_ty().clone();
        let ty = self.ty.clone();
        func_list.insert(self.name.clone(), self);
        ty
    }
}


impl ExprIr {
    fn ty_check(
        &mut self,
        func_list: &mut HashMap<String, FuncIr>,
        ex_func_list: &ExFuncList,
        params: &mut Vec<Type>,
        expect_ty: Type,
    ) -> Type {
        match self {
            ExprIr::NumIr(x) => x.ty_check(),
            ExprIr::CallIr(x) => x.ty_check(func_list, ex_func_list, params, expect_ty),
            ExprIr::OpIr(x) => x.ty_check(func_list, ex_func_list, params),
            ExprIr::VariableIr(x) => x.ty_check(params, expect_ty),
            ExprIr::GlobalVariableIr(x) => x.ty_check(func_list, ex_func_list, expect_ty),
        }
    }
}

impl NumIr {
    fn ty_check(&mut self) -> Type {
        if self.ty == Type::Unknown {
            self.ty = Type::Int32;
        };
        self.ty.clone()
    }
}

impl CallIr {
    fn ty_check(
        &mut self,
        func_list: &mut FuncList,
        ex_func_list: &ExFuncList,
        params: &mut Vec<Type>,
        expect_ty: Type,
    ) -> Type {
        for i in 0..self.params.len() {
            self.params[i].ty_check(func_list, ex_func_list, params, Type::Unknown);
        }
        let expect_func_ty = Type::create_fn_func_type(
            self.params.iter().map(|x| x.get_ty().clone()).collect(),
            expect_ty.clone(),
        );

        self.func.ty_check(func_list, ex_func_list, params, expect_func_ty);

        match self.func.get_ty() {
            Type::Fn(x) => self.ty = x.ret_type.clone(),
            _ => panic!("type error!")
        };

        self.ty.clone()
    }
}

impl OpIr {
    fn ty_check(&mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, params: &mut Vec<Type>) -> Type {
        if self.ty != Type::Unknown {
            return self.ty.clone();
        }
        if self.l_expr.ty_check(func_list, ex_func_list, params, Type::Int32) == Type::Int32
            && self.r_expr.ty_check(func_list, ex_func_list, params, Type::Int32) == Type::Int32 {
            self.ty = Type::Int32;
        } else { panic!("type error!"); }
        self.ty.clone()
    }
}

impl VariableIr {
    fn ty_check(&mut self, params: &mut Vec<Type>, expect_ty: Type) -> Type {
        let mut param = params[params.len() - self.id - 1].clone();
        match param.merge(expect_ty) {
            Option::Some(ty) => param = ty,
            _ => panic!("type error!")
        }
        let len = params.len();
        params[len - self.id - 1] = param.clone();
        self.ty = param;
        self.ty.clone()
    }
}

impl GlobalVariableIr {
    fn ty_check(&mut self, func_list: &mut FuncList, ex_func_list: &ExFuncList, expect_ty: Type) -> Type {
        let func_ty =
            match func_list.remove(&self.id) {
                Some(x) => x.ty_check(func_list, ex_func_list, expect_ty),
                _ =>
                    match ex_func_list.get(&self.id) {
                        Some(x) => x.ty_check(expect_ty),
                        _ => panic!("call not found function")
                    }
            };
        self.ty = Type::Fn(Box::new(func_ty));
        self.ty.clone()
    }
}