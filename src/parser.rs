use crate::lexer::{parse_input, TokenIter};
// all of the possible Abstract Syntax Tree node expressions
enum ExprAST<'a> {
    Number(NumberExpr),
    Variable(VariableExpr<'a>),
    BinaryOp(&'a BinaryOpExpr<'a>),
    CallOp(CallOpExpr<'a>),
    FnType(FnTypeExpr<'a>),
    Function(&'a FunctionExpr<'a>),
}

struct NumberExpr {
    value: f64,
}

struct VariableExpr<'a> {
    name: &'a str,
}

struct BinaryOpExpr<'a> {
    op: char,
    args: [ExprAST<'a>; 2],
}

struct CallOpExpr<'a> {
    callee: &'a str,
    args: Vec<ExprAST<'a>>,
}

struct FnTypeExpr<'a> {
    name: &'a str,
    args: Vec<&'a str>,
}

struct FunctionExpr<'a> {
    ty: FnTypeExpr<'a>,
    body: ExprAST<'a>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::Peekable;

    #[test]
    fn test_binop() {
        let input = "x + y";
        let mut tok_iter = TokenIter::new(&input).peekable();

        let lhs = ExprAST::Variable(VariableExpr { name: "x" });
        let rhs = ExprAST::Variable(VariableExpr { name: "y" });

        let result = BinaryOpExpr {
            op: '+',
            args: [lhs, rhs],
        };
    }
}
