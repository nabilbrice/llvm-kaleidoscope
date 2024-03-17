use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::FloatValue;

use crate::parser::{ExprAST, NumberExpr, VariableExpr};

// not really the compiler but actually the current scope
struct Compiler<'a> {
    llvm_context: &'a Context,

    // these are (function passed) variables,
    // which should be given some FloatValue already
    variables: HashMap<&'a str, FloatValue<'a>>,
}

impl<'a> Compiler<'a> {
    fn float_codegen(self: &Self, numexpr: NumberExpr) -> FloatValue<'a> {
        self.llvm_context.f64_type().const_float(numexpr.value)
    }

    // gets the FloatValue stored in the hashmap of the compiler
    fn var_codegen(self: &Self, varexpr: VariableExpr) -> Option<&FloatValue<'a>> {
        self.variables.get(varexpr.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenIter;
    use crate::parser::make_primitive;

    #[test]
    fn test_float_gen() {
        let input = "2.0";
        let mut tokenstream = TokenIter::new(&input).peekable();
        let prim_2 = match make_primitive(tokenstream.next().as_ref()) {
            Some(ExprAST::Number(tok)) => tok,
            _ => panic!("Uh oh!"),
        };

        let llvm_context = Context::create();
        let compiler = Compiler {
            llvm_context: &llvm_context,
            variables: HashMap::new(),
        };

        let result = llvm_context.f64_type().const_float(2.0);

        let code = compiler.float_codegen(prim_2);

        dbg!(code);
        dbg!(result);
        assert_eq!(result, code);
    }
}
