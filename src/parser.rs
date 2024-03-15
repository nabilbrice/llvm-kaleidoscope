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

// A primitive AST node is without any sub-nodes
fn make_primitive<'a>(tok: &Token<'a>) -> ExprAST<'a> {
    match tok {
        Token::Identifier(str) => ExprAST::Variable(VariableExpr { name: str }),
        Token::Number(value) => ExprAST::Number(NumberExpr {
            value: value.clone(),
        }),
        _ => panic!("Expected primitive!"),
    }
}

// should be impl for the Token?
fn get_opcode<'a>(tok: &Token<'a>) -> Option<char> {
    match tok {
        Token::Identifier("+") => Some('+'),
        Token::Identifier("-") => Some('-'),
        Token::Identifier("*") => Some('*'),
        Token::Identifier("<") => Some('<'),
        _ => None,
    }
}

// should be impl for the TokenIter?
// almost all commands in a program are actually binary operations
// the basic commands of a asm language are usually binop:
// move, add, load
fn make_expr<'a>(tok_iter: &'a mut Peekable<TokenIter>) -> ExprAST<'a> {
    let lhs = make_primitive(&tok_iter.next().unwrap());

    match get_opcode(tok_iter.peek().expect("Reached end!")) {
        Some(opcode) => {
            tok_iter.next();
            let rhs = make_expr(tok_iter);
            ExprAST::BinaryOp(Box::new(BinaryOpExpr {
                op: opcode,
                args: [lhs, rhs],
            }))
        }
        None => lhs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_binop() {
        let input = "x - 2.0";
        let mut tok_iter = TokenIter::new(&input).peekable();
        let mut tmp_iter = TokenIter::new(&input).peekable();

        let lhs = make_primitive(tok_iter.peek().unwrap());
        assert_eq!(ExprAST::Variable(VariableExpr { name: "x" }), lhs);
        tok_iter.next();
        tok_iter.next();
        let rhs = make_primitive(tok_iter.peek().unwrap());
        assert_eq!(ExprAST::Number(NumberExpr { value: 2.0 }), rhs);

        let result = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '-',
            args: [lhs, rhs],
        }));
        let binop = make_expr(&mut tmp_iter);
        assert_eq!(result, binop)
    }

    #[test]
    fn test_complex_binop() {
        let input = "x + y - 2.0";
        let mut tok_iter = TokenIter::new(&input).peekable();
        let mut tmp_iter = TokenIter::new(&input).peekable();

        let prim_x = make_primitive(tmp_iter.peek().unwrap());
        assert_eq!(ExprAST::Variable(VariableExpr { name: "x" }), prim_x);
        tmp_iter.next();
        tmp_iter.next();
        let prim_y = make_primitive(tmp_iter.peek().unwrap());
        assert_eq!(ExprAST::Variable(VariableExpr { name: "y" }), prim_y);

        tmp_iter.next();
        tmp_iter.next();
        let prim_r = make_primitive(tmp_iter.peek().unwrap());
        assert_eq!(ExprAST::Number(NumberExpr { value: 2.0 }), prim_r);

        let expr = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '-',
            args: [prim_y, prim_r],
        }));

        let result = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '+',
            args: [prim_x, expr],
        }));
        let binop = make_expr(&mut tok_iter);
        assert_eq!(result, binop);
    }
}
