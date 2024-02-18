use crate::lexer::{parse_input, Token, TokenIter};
use std::iter::Peekable;
// all of the possible Abstract Syntax Tree node expressions
#[derive(Debug, PartialEq)]
enum ExprAST<'a> {
    Number(NumberExpr),
    Variable(VariableExpr<'a>),
    BinaryOp(Box<BinaryOpExpr<'a>>),
    CallOp(CallOpExpr<'a>),
    FnType(FnTypeExpr<'a>),
    Function(Box<FunctionExpr<'a>>),
}

#[derive(Debug, PartialEq)]
struct NumberExpr {
    value: f64,
}

#[derive(Debug, PartialEq)]
struct VariableExpr<'a> {
    name: &'a str,
}

#[derive(Debug, PartialEq)]
struct BinaryOpExpr<'a> {
    op: char,
    args: [ExprAST<'a>; 2],
}

#[derive(Debug, PartialEq)]
struct CallOpExpr<'a> {
    callee: &'a str,
    args: Vec<ExprAST<'a>>,
}

#[derive(Debug, PartialEq)]
struct FnTypeExpr<'a> {
    name: &'a str,
    args: Vec<&'a str>,
}

#[derive(Debug, PartialEq)]
struct FunctionExpr<'a> {
    ty: FnTypeExpr<'a>,
    body: ExprAST<'a>,
}

// should be impl for the Token?
fn make_variable<'a>(tok: &Token<'a>) -> ExprAST<'a> {
    match tok {
        Token::Identifier(str) => ExprAST::Variable(VariableExpr { name: str }),
        _ => panic!("Variable not parsed!"),
    }
}

// should be impl for the Token?
fn get_opcode<'a>(tok: &Token<'a>) -> Option<char> {
    match tok {
        Token::Identifier(opcode) => opcode.chars().next(),
        _ => panic!("Error in retrieving opcode!"),
    }
}

// should be impl for the TokenIter?
fn make_binop<'a>(tok_iter: &'a mut Peekable<TokenIter>) -> ExprAST<'a> {
    let lhs = make_variable(&tok_iter.next().unwrap());
    // peek to the next to see if it is a binop:
    let op = get_opcode(tok_iter.peek().unwrap());
    tok_iter.next();
    let rhs = make_variable(tok_iter.peek().unwrap());

    ExprAST::BinaryOp(Box::new(BinaryOpExpr {
        op: op.unwrap(),
        args: [lhs, rhs],
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binop() {
        let input = "x + y";
        let mut tok_iter = TokenIter::new(&input).peekable();
        let mut tmp_iter = TokenIter::new(&input).peekable();

        let lhs = make_variable(tok_iter.peek().unwrap());
        assert_eq!(ExprAST::Variable(VariableExpr { name: "x" }), lhs);
        tok_iter.next();
        tok_iter.next();
        let rhs = make_variable(tok_iter.peek().unwrap());
        assert_eq!(ExprAST::Variable(VariableExpr { name: "y" }), rhs);

        let result = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '+',
            args: [lhs, rhs],
        }));
        let binop = make_binop(&mut tmp_iter);
        assert_eq!(result, binop)
    }
}
