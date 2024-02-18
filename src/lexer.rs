use std::{fmt::Error, num::ParseFloatError};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    EOF,
    // commands
    Def,
    Extern,
    // primary
    Identifier(&'a str),
    Number(f64),
}

// An iterator state holder for tokens
pub struct TokenIter<'a> {
    tok: Token<'a>,
    remainder: &'a str,
}

impl<'a> TokenIter<'a> {
    pub fn new(input: &'a str) -> TokenIter {
        TokenIter {
            // the initial token has to be set to something
            // maybe EOF can be replaced with None instead
            tok: Token::EOF,
            remainder: input,
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        (self.tok, self.remainder) = parse_input(self.remainder);
        Some(self.tok.clone())
    }
}

// TODO: ensure the last token in a file can be parsed!
// parse tokens through a view of the whole input
// only a view is needed because parsing into the language tokens
// does not need to mutate the input string
pub fn parse_input<'a>(input: &'a str) -> (Token<'a>, &'a str) {
    // the view of the whole input, trimming the whitespace start
    let substr = input.trim_start();
    // the counter for where to split later
    // this actually already tracks the state
    let mut split_index: usize = 0;
    let split: (&'a str, &'a str);
    // handle the condition when the chars.next() is None
    if substr.is_empty() {
        return (Token::EOF, input);
    };
    // needed to keep track of the state again:
    // the state is tracked again here and needs to be synced
    let mut chars = substr.chars();
    chars.next();
    split_index += 1;
    // if the substring starts with a numeric char
    // this checks whether the next char is also numeric
    // and updates the position of the split_index if it is
    // really it is finding the end
    if substr.starts_with(char::is_numeric) {
        let mut c = chars.next();
        // necessary to avoid stopping at a decimal point
        while c.is_some_and(char::is_numeric) || c == Some('.') {
            split_index += 1;
            c = chars.next();
        }
    } else if substr.starts_with(char::is_alphabetic) {
        let mut c = chars.next();
        while c.is_some_and(char::is_alphanumeric) {
            split_index += 1;
            c = chars.next();
        }
    };
    // the state of the window has to be managed directly
    split = substr.split_at(split_index);
    (parse_token(split.0).unwrap(), split.1)
}

// This function takes a string slice split by whitespaces
fn parse_token<'a>(str_view: &'a str) -> Result<Token<'a>, ParseFloatError> {
    match str_view {
        "def" => Ok(Token::Def),
        "extern" => Ok(Token::Extern),
        other => {
            if other.starts_with(char::is_numeric) {
                Ok(Token::Number(other.parse::<f64>()?))
            } else {
                Ok(Token::Identifier(other))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_splitting() {
        let input = "Consider! + These_tokens.".to_string();
        let mut split = input.split_whitespace();

        assert_eq!("Consider!", split.next().unwrap());
        assert_eq!("+", split.next().unwrap());
        assert_eq!("These_tokens.", split.next().unwrap());
    }

    #[test]
    fn test_consuming() {
        let input = "Consider! + These_tokens.".to_string();
        let chars = input.chars();
    }

    #[test]
    fn test_parse_token() {
        let input = "defin+def  def Extern extern".to_string();
        let mut split = input.split_whitespace();

        assert_eq!(
            Ok(Token::Identifier("defin+def")),
            parse_token(split.next().unwrap())
        );
        assert_eq!(Ok(Token::Def), parse_token(split.next().unwrap()));
        assert_eq!(
            Ok(Token::Identifier("Extern")),
            parse_token(split.next().unwrap())
        );
        assert_eq!(Ok(Token::Extern), parse_token(split.next().unwrap()));
        // The call to split.next() will give None
    }

    #[test]
    fn test_split_numerical() {
        let input = "defin+def1 0.1 1 1df def Extern extern".to_string();
        let mut split = input.split_whitespace();

        assert_eq!(
            Ok(Token::Identifier("defin+def1")),
            parse_token(split.next().unwrap())
        );
        assert_eq!(Ok(Token::Number(0.1)), parse_token(split.next().unwrap()));
        assert_eq!(Ok(Token::Number(1.0)), parse_token(split.next().unwrap()));
    }

    #[test]
    fn test_parse_input() {
        let input = "defin+def1 0.1 def Extern extern".to_string();
        let parse_once = parse_input(&input);
        let parse_twice = parse_input(&parse_once.1);
        let parse_thrice = parse_input(&parse_twice.1);
        let parse_quattro = parse_input(&parse_thrice.1);
        let parse_quinto = parse_input(&parse_quattro.1);
        assert_eq!(Token::Identifier("defin"), parse_once.0);
        assert_eq!(Token::Identifier("+"), parse_twice.0);
        assert_eq!(Token::Identifier("def1"), parse_thrice.0);
        assert_eq!(Token::Number(0.1), parse_quattro.0);
        assert_eq!(Token::Def, parse_quinto.0);
    }

    #[test]
    fn test_token_iter() {
        let input = "defin+def1 0.1 def Extern extern".to_string();
        let mut tok_iter = TokenIter::new(&input);
        assert_eq!(Some(Token::Identifier("defin")), tok_iter.next());
        assert_eq!(Some(Token::Identifier("+")), tok_iter.next());
        assert_eq!(Some(Token::Identifier("def1")), tok_iter.next());
        assert_eq!(Some(Token::Number(0.1)), tok_iter.next());
        assert_eq!(Some(Token::Def), tok_iter.next());
        assert_eq!(Some(Token::Identifier("Extern")), tok_iter.next());
        assert_eq!(Some(Token::Extern), tok_iter.next());
        assert_eq!(Some(Token::EOF), tok_iter.next());
        // from here on the token produced should be Token::EOF
        assert_eq!(Some(Token::EOF), tok_iter.next());
    }
}
