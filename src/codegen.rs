use std::collections::HashMap;

use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FloatValue;

use crate::parser::{BinaryOpExpr, ExprAST, NumberExpr, VariableExpr};

// not really the compiler but actually the current scope
struct Compiler<'a> {
    llvm_context: &'a Context,
    llvm_builder: Builder<'a>,
    llvm_module: Module<'a>,

    // these are (function passed) variables,
    // which should be given some FloatValue already
    variables: HashMap<&'a str, FloatValue<'a>>,
}

impl<'a> Compiler<'a> {
    fn float_codegen(self: &Self, expr: &ExprAST<'a>) -> FloatValue<'a> {
        match expr {
            ExprAST::Number(numexpr) => self.llvm_context.f64_type().const_float(numexpr.value),
            _ => panic!("Oh no!"),
        }
    }

    // gets the FloatValue stored in the hashmap of the compiler
    fn var_codegen(self: &Self, varexpr: VariableExpr) -> Option<&FloatValue<'a>> {
        self.variables.get(varexpr.name)
    }

    // this can only be called after a basic block has been added
    // which is why a top level expression is an anonymous function
    fn binop_codegen(self: &Self, binop: BinaryOpExpr<'a>) -> Result<FloatValue, BuilderError> {
        let lhs = self.float_codegen(&binop.args[0]);
        let rhs = self.float_codegen(&binop.args[1]);
        match binop.op {
            '+' => self.llvm_builder.build_float_add(lhs, rhs, "addtmp"),
            '-' => self.llvm_builder.build_float_sub(lhs, rhs, "subtmp"),
            '*' => self.llvm_builder.build_float_mul(lhs, rhs, "multmp"),
            '<' => {
                let uintval = self.llvm_builder.build_float_compare(
                    inkwell::FloatPredicate::OLT,
                    lhs,
                    rhs,
                    "cmptmp",
                );
                self.llvm_builder.build_unsigned_int_to_float(
                    uintval?,
                    self.llvm_context.f64_type(),
                    "cmptmp",
                )
            }
            _ => panic!("Uh oh!"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenIter;
    use crate::parser::{make_expr, make_primitive};

    #[test]
    fn test_float_gen() {
        let input = "2.0";
        let mut tokenstream = TokenIter::new(&input).peekable();
        let prim_2 = make_primitive(tokenstream.next().as_ref()).unwrap();

        let llvm_context = Context::create();
        let compiler = Compiler {
            llvm_context: &llvm_context,
            llvm_builder: llvm_context.create_builder(),
            llvm_module: llvm_context.create_module("empty"),
            variables: HashMap::new(),
        };

        let result = llvm_context.f64_type().const_float(2.0);

        let code = compiler.float_codegen(&prim_2);
        assert_eq!(result, code);
    }

    #[test]
    fn test_adder_gen() {
        let input = "2.0 < 5.0";
        let mut tokenstream = TokenIter::new(&input).peekable();
        let binop = match make_expr(&mut tokenstream, 0) {
            ExprAST::BinaryOp(binopexpr) => binopexpr,
            _ => panic!("Oh no!"),
        };

        let llvm_context = Context::create();
        let compiler = Compiler {
            llvm_context: &llvm_context,
            llvm_builder: llvm_context.create_builder(),
            llvm_module: llvm_context.create_module("sum_test"),
            variables: HashMap::new(),
        };

        // a workaround for top level expression an anonymous function
        let anon_fn_type = llvm_context.f64_type().fn_type(&[], false);
        let function = compiler
            .llvm_module
            .add_function("anon_sum", anon_fn_type, None);
        let basic_block = compiler.llvm_context.append_basic_block(function, "entry");
        compiler.llvm_builder.position_at_end(basic_block);

        let code = compiler.binop_codegen(*binop).unwrap();
        println!("{code}");
    }
}
