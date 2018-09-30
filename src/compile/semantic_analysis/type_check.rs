use std::collections::HashMap;
use super::super::types;
use super::ir;

impl ir::ProgramIr {
    pub fn ty_check(mut self) -> ir::ProgramIr {
        let func_name_list: Vec<String> =
            self.func_list.values().clone()
                .into_iter().map(|funcir| {
                funcir.name.clone()
            }).collect();
        for func_name in func_name_list {
            let func_ir = self.func_list.remove(&func_name).unwrap();
            func_ir.ty_check(&mut self.func_list, &self.extern_func_list, types::Type::Unknown);
        }
        self
    }
}

type FuncList = HashMap<String, ir::FuncIr>;
type ExternFuncList = HashMap<String, ir::DecFuncIr>;

impl ir::DecFuncIr {
    fn ty_check(&self, expect_ty: types::Type) -> types::FuncType {
        match expect_ty {
            types::Type::Unknown => self.ty.clone(),
            types::Type::Fn(x) =>
                if self.ty >= *x {
                    self.ty.clone()
                } else {
                    panic!("type error!")
                }
            _ => panic!("type error!")
        }
    }
}

impl ir::FuncIr {
    fn ty_check(mut self, func_list: &mut FuncList, extern_func_list: &ExternFuncList, expect_ty: types::Type) -> types::FuncType {
        match match expect_ty {
            types::Type::Fn(x) => self.ty.merge(types::Type::FuncType(Box::new(*x))),
            x => self.ty.merge(x)
        }
            {
                Some(x) => self.ty = x,
                _ => panic!("type error!")
            };
        self.body.ty_check(func_list, extern_func_list, &mut self.ty.param_types, self.ty.ret_type);
        self.ty.ret_type = self.body.get_ty().clone();
        let ty = self.ty.clone();
        func_list.insert(self.name.clone(), self);
        ty
    }
}


impl ir::ExprIr {
    fn ty_check(
        &mut self,
        func_list: &mut HashMap<String, ir::FuncIr>,
        extern_func_list: &ExternFuncList,
        params: &mut Vec<types::Type>,
        expect_ty: types::Type,
    ) -> types::Type {
        match self {
            ir::ExprIr::NumIr(x) => x.ty_check(),
            ir::ExprIr::CallIr(x) => x.ty_check(func_list, extern_func_list, params, expect_ty),
            ir::ExprIr::OpIr(x) => x.ty_check(func_list, extern_func_list, params),
            ir::ExprIr::VariableIr(x) => x.ty_check(params, expect_ty),
            ir::ExprIr::GlobalVariableIr(x) => x.ty_check(func_list, extern_func_list, expect_ty),
        }
    }
}

impl ir::NumIr {
    fn ty_check(&mut self) -> types::Type {
        if self.ty == types::Type::Unknown {
            self.ty = types::Type::Int32;
        };
        self.ty.clone()
    }
}

impl ir::CallIr {
    fn ty_check(
        &mut self,
        func_list: &mut FuncList,
        extern_func_list: &ExternFuncList,
        params: &mut Vec<types::Type>,
        expect_ty: types::Type,
    ) -> types::Type {
        for i in 0..self.params.len() {
            self.params[i].ty_check(func_list, extern_func_list, params, types::Type::Unknown);
        }
        let expect_func_ty = types::Type::create_fn_func_type(
            self.params.iter().map(|x| x.get_ty().clone()).collect(),
            expect_ty.clone(),
        );

        self.func.ty_check(func_list, extern_func_list, params, expect_func_ty);

        match self.func.get_ty() {
            types::Type::Fn(x) => self.ty = x.ret_type.clone(),
            _ => panic!("type error!")
        };

        self.ty.clone()
    }
}

impl ir::OpIr {
    fn ty_check(&mut self, func_list: &mut FuncList, extern_func_list: &ExternFuncList, params: &mut Vec<types::Type>) -> types::Type {
        if self.ty != types::Type::Unknown {
            return self.ty.clone();
        }
        if self.l_expr.ty_check(func_list, extern_func_list, params, types::Type::Int32) == types::Type::Int32
            && self.r_expr.ty_check(func_list, extern_func_list, params, types::Type::Int32) == types::Type::Int32 {
            self.ty = types::Type::Int32;
        } else { panic!("type error!"); }
        self.ty.clone()
    }
}

impl ir::VariableIr {
    fn ty_check(&mut self, params: &mut Vec<types::Type>, expect_ty: types::Type) -> types::Type {
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

impl ir::GlobalVariableIr {
    fn ty_check(&mut self, func_list: &mut FuncList, extern_func_list: &ExternFuncList, expect_ty: types::Type) -> types::Type {
        let func_ty =
            match func_list.remove(&self.id) {
                Some(x) => x.ty_check(func_list, extern_func_list, expect_ty),
                _ =>
                    match extern_func_list.get(&self.id) {
                        Some(x) => x.ty_check(expect_ty),
                        _ => panic!("call not found function")
                    }
            };
        self.ty = types::Type::Fn(Box::new(func_ty));
        self.ty.clone()
    }
}