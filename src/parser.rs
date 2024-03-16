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
    args: Vec<VariableExpr<'a>>,
}

#[derive(Debug, PartialEq)]
struct FunctionExpr<'a> {
    ty: FnTypeExpr<'a>,
    body: ExprAST<'a>,
}

// A primitive AST node is without any sub-nodes
fn make_primitive<'a>(token: Option<&Token<'a>>) -> Option<ExprAST<'a>> {
    match token {
        Some(Token::Identifier(str)) => Some(ExprAST::Variable(VariableExpr { name: str })),
        Some(Token::Number(value)) => Some(ExprAST::Number(NumberExpr {
            value: value.clone(),
        })),
        _ => None,
    }
}

// should be impl for the Token?
fn get_opcode<'a>(token: Option<&Token<'a>>) -> (char, i32) {
    match token {
        Some(Token::Identifier("+")) => ('+', 50),
        Some(Token::Identifier("-")) => ('-', 40),
        Some(Token::Identifier("*")) => ('*', 80),
        Some(Token::Identifier("<")) => ('<', 100),
        // default
        _ => ('#', -1000),
    }
}

// should be impl for the TokenIter?
// almost all commands in a program are actually binary operations
// the basic commands of a asm language are usually binop:
// move, add, load
// the prec is the precendence binding value of the operation
// this currently only makes an expression with all rhs expanded
// a parser needs to construct the total AST
fn make_expr<'a>(tokenstream: &mut Peekable<TokenIter<'a>>, prec: i32) -> ExprAST<'a> {
    let mut lhs = make_primitive(tokenstream.next().as_ref());

    loop {
        let (op, new_prec) = get_opcode(tokenstream.peek());
        // if the new precedence binding is lower
        // it's time to break out of the loop
        if new_prec < prec {
            break;
        };
        tokenstream.next();
        let rhs = make_expr(tokenstream, new_prec);
        lhs = Some(ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op,
            args: [lhs.unwrap(), rhs],
        })));
    }
    lhs.unwrap()
}

fn make_signature<'a>(tokenstream: &mut Peekable<TokenIter<'a>>) -> FnTypeExpr<'a> {
    let name = match tokenstream.next() {
        Some(Token::Identifier(str)) => str,
        _ => panic!("Unparseable name of function"),
    };
    match tokenstream.next() {
        Some(Token::Identifier("(")) => {}
        Some(Token::Identifier(str)) => eprint!("Expected '(', found {}", str),
        _ => panic!("Unable to parse arglist for {}", name),
    };
    // prepare the arglist
    let mut arglist = Vec::<VariableExpr<'a>>::new();
    loop {
        match tokenstream.next() {
            Some(Token::Identifier(str)) => arglist.push(VariableExpr { name: str }),
            _ => panic!("Error parsing arglist!"),
        };
        match tokenstream.next() {
            Some(Token::Identifier(")")) => break,
            Some(Token::Identifier(",")) => continue,
            _ => panic!("Something horrible has happened!"),
        };
    }
    FnTypeExpr {
        name,
        args: arglist,
    }
}

fn make_function<'a>(tokenstream: &mut Peekable<TokenIter<'a>>) -> FunctionExpr<'a> {
    let ty = make_signature(tokenstream);
    let body = make_expr(tokenstream, 0);
    FunctionExpr { ty, body }
}

// parsers a top level expression into an anonymous function
fn make_topexpr<'a>(tokenstream: &mut Peekable<TokenIter<'a>>) -> FunctionExpr<'a> {
    let topty = FnTypeExpr {
        name: "",
        args: Vec::new(),
    };
    let body = make_expr(tokenstream, 0);
    FunctionExpr { ty: topty, body }
}

fn make_ast<'a>(tokenstream: &mut Peekable<TokenIter<'a>>) -> ExprAST<'a> {
    match tokenstream.peek() {
        Some(Token::Def) => {
            tokenstream.next();
            ExprAST::Function(Box::new(make_function(tokenstream)))
        }
        Some(Token::Extern) => {
            tokenstream.next();
            ExprAST::Function(Box::new(make_function(tokenstream)))
        }
        _ => ExprAST::Function(Box::new(make_topexpr(tokenstream))),
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

        let lhs = make_primitive(tok_iter.peek()).unwrap();
        assert_eq!(ExprAST::Variable(VariableExpr { name: "x" }), lhs);
        tok_iter.next();
        tok_iter.next();
        let rhs = make_primitive(tok_iter.peek()).unwrap();
        assert_eq!(ExprAST::Number(NumberExpr { value: 2.0 }), rhs);

        let result = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '-',
            args: [lhs, rhs],
        }));
        let binop = make_expr(&mut tmp_iter, 0);
        assert_eq!(result, binop)
    }

    #[test]
    fn test_complex_binop() {
        let input = "x + y - 2.0";
        let mut tok_iter = TokenIter::new(&input).peekable();
        let mut tmp_iter = TokenIter::new(&input).peekable();

        let prim_x = make_primitive(tmp_iter.peek()).unwrap();
        assert_eq!(ExprAST::Variable(VariableExpr { name: "x" }), prim_x);
        tmp_iter.next();
        tmp_iter.next();
        let prim_y = make_primitive(tmp_iter.peek()).unwrap();
        assert_eq!(ExprAST::Variable(VariableExpr { name: "y" }), prim_y);

        tmp_iter.next();
        tmp_iter.next();
        let prim_r = make_primitive(tmp_iter.peek()).unwrap();
        assert_eq!(ExprAST::Number(NumberExpr { value: 2.0 }), prim_r);

        let lhs = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '+',
            args: [prim_x, prim_y],
        }));
        let result = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '-',
            args: [lhs, prim_r],
        }));
        let binop = make_expr(&mut tok_iter, 0);
        assert_eq!(result, binop);
    }

    #[test]
    fn test_fndef() {
        let input = "def multiply(x,y) x*y";
        let mut tokenstream = TokenIter::new(&input).peekable();

        let var_x = VariableExpr { name: "x" };
        let var_y = VariableExpr { name: "y" };

        let fntype = FnTypeExpr {
            name: "multiply",
            args: vec![var_x, var_y],
        };

        let prim_x = ExprAST::Variable(VariableExpr { name: "x" });
        let prim_y = ExprAST::Variable(VariableExpr { name: "y" });

        let fnbody = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '*',
            args: [prim_x, prim_y],
        }));

        let result = ExprAST::Function(Box::new(FunctionExpr {
            ty: fntype,
            body: fnbody,
        }));

        let function = make_ast(&mut tokenstream);
        assert_eq!(result, function);
    }

    #[test]
    fn test_toplevel_expression() {
        let input = "x - 2.0";
        let mut tokenstream = TokenIter::new(&input).peekable();

        let anonty = FnTypeExpr {
            name: "",
            args: Vec::new(),
        };
        let prim_x = ExprAST::Variable(VariableExpr { name: "x" });
        let prim_2 = ExprAST::Number(NumberExpr { value: 2.0 });
        let body = ExprAST::BinaryOp(Box::new(BinaryOpExpr {
            op: '-',
            args: [prim_x, prim_2],
        }));
        let result = ExprAST::Function(Box::new(FunctionExpr { ty: anonty, body }));

        let toplevel = make_ast(&mut tokenstream);
        assert_eq!(result, toplevel);
    }
}
