use std::{
    ops::{Add, Div, Mul, Sub},
    str::FromStr,
};

macro_rules! apply_op {
    ($op:ident, $tokens:ident, $idx:ident) => {{
        $tokens[$idx - 1] =
            Token::Num($tokens[$idx - 1].as_num()?.$op($tokens[$idx + 1].as_num()?));
        $tokens.remove($idx + 1);
        $tokens.remove($idx);
    }};
}

#[derive(Debug)]
enum CalcError {
    Op,
    Input,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl FromStr for Op {
    type Err = CalcError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Op::Add),
            "-" => Ok(Op::Sub),
            "*" => Ok(Op::Mul),
            "/" => Ok(Op::Div),
            _ => Err(CalcError::Input),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Token {
    Num(i64),
    Op(Op),
}

impl Token {
    fn as_num(&mut self) -> Result<i64, CalcError> {
        match self {
            Token::Num(x) => Ok(*x),
            _ => Err(CalcError::Op),
        }
    }
}

impl FromStr for Token {
    type Err = CalcError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(x) = s.parse::<i64>() {
            return Ok(Token::Num(x));
        } else {
            let op = Op::from_str(s)?;
            return Ok(Token::Op(op));
        }
    }
}

fn eval_str(s: &str) -> Result<i64, CalcError> {
    let mut tokens = tokenize(s)?;

    let mut i = 0;

    // do mul/div
    loop {
        // check if there are any
        let ops: Vec<&Token> = tokens
            .iter()
            .filter(|e| **e == Token::Op(Op::Div) || **e == Token::Op(Op::Mul))
            .collect();
        if ops.is_empty() {
            break;
        }

        if let Some(token) = tokens.get(i) {
            if let Token::Op(op) = *token {
                match op {
                    Op::Div => apply_op!(div, tokens, i),
                    Op::Mul => apply_op!(mul, tokens, i),
                    _ => {}
                }
            }
            i += 1;
        } else {
            // start again from the top
            i = 0;
        }
    }

    i = 0;
    // do add/sub
    loop {
        // check if there are any
        let ops: Vec<&Token> = tokens
            .iter()
            .filter(|e| **e == Token::Op(Op::Add) || **e == Token::Op(Op::Sub))
            .collect();
        if ops.is_empty() {
            break;
        }
        if let Some(token) = tokens.get(i) {
            if let Token::Op(op) = *token {
                match op {
                    Op::Add => apply_op!(add, tokens, i),
                    Op::Sub => apply_op!(sub, tokens, i),
                    _ => {}
                }
            }
            i += 1;
        } else {
            // start again from the top
            i = 0;
        }
    }

    // should only be one token left, return it
    Ok(tokens[0].as_num()?)
}

// string to token Vec
fn tokenize(s: &str) -> Result<Vec<Token>, CalcError> {
    let mut ret = Vec::new();
    for c in s.split(' ') {
        ret.push(Token::from_str(c)?);
    }
    Ok(ret)
}

fn main() {
    let input = "2 / 2 + 3 * 4 - 6";
    assert_eq!(eval_str(input).unwrap(), 7);
}