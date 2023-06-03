use crate::token::{self, Token};

#[derive(Clone)]
pub struct Lexer {
    input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char,             // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let mut tok = Token {
            token_type: String::new(),
            literal: String::new(),
        };

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch);
                    tok = token::Token {
                        token_type: token::EQ.to_string(),
                        literal,
                    };
                } else {
                    tok = self.new_token(token::ASSIGN.to_string(), self.ch)
                }
            }
            ';' => tok = self.new_token(token::SEMICOLON.to_string(), self.ch),
            '(' => tok = self.new_token(token::LPAREN.to_string(), self.ch),
            ')' => tok = self.new_token(token::RPAREN.to_string(), self.ch),
            ',' => tok = self.new_token(token::COMMA.to_string(), self.ch),
            '+' => tok = self.new_token(token::PLUS.to_string(), self.ch),
            '-' => tok = self.new_token(token::MINUS.to_string(), self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch);
                    tok = token::Token {
                        token_type: token::NOT_EQ.to_string(),
                        literal,
                    };
                } else {
                    tok = self.new_token(token::BANG.to_string(), self.ch)
                }
            }
            '/' => tok = self.new_token(token::SLASH.to_string(), self.ch),
            '*' => tok = self.new_token(token::ASTERISK.to_string(), self.ch),
            '<' => tok = self.new_token(token::LT.to_string(), self.ch),
            '>' => tok = self.new_token(token::GT.to_string(), self.ch),
            '{' => tok = self.new_token(token::LBRACE.to_string(), self.ch),
            '}' => tok = self.new_token(token::RBRACE.to_string(), self.ch),
            '\0' => {
                tok.literal = String::new();
                tok.token_type = token::EOF.to_string();
            }
            _ => {
                if is_letter(self.ch) {
                    tok.literal = self.read_identifier();
                    tok.token_type = token::lookup_ident(&tok.literal);
                    return tok;
                } else if is_digit(self.ch) {
                    tok.token_type = token::INT.to_string();
                    tok.literal = self.read_number();
                    return tok;
                } else {
                    tok = self.new_token(token::ILLEGAL.to_string(), self.ch);
                }
            }
        }

        self.read_char();
        tok
    }

    fn new_token(&mut self, token_type: String, ch: char) -> Token {
        Token {
            token_type,
            literal: ch.to_string(),
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }
}

fn is_letter(ch: char) -> bool {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use crate::token::{self};

    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
return true;
} else {
return false;
}
10 == 10;
10 != 9;
:
"#;

        let tests = [
            (token::LET.to_string(), "let"),
            (token::IDENT.to_string(), "five"),
            (token::ASSIGN.to_string(), "="),
            (token::INT.to_string(), "5"),
            (token::SEMICOLON.to_string(), ";"),
            (token::LET.to_string(), "let"),
            (token::IDENT.to_string(), "ten"),
            (token::ASSIGN.to_string(), "="),
            (token::INT.to_string(), "10"),
            (token::SEMICOLON.to_string(), ";"),
            (token::LET.to_string(), "let"),
            (token::IDENT.to_string(), "add"),
            (token::ASSIGN.to_string(), "="),
            (token::FUNCTION.to_string(), "fn"),
            (token::LPAREN.to_string(), "("),
            (token::IDENT.to_string(), "x"),
            (token::COMMA.to_string(), ","),
            (token::IDENT.to_string(), "y"),
            (token::RPAREN.to_string(), ")"),
            (token::LBRACE.to_string(), "{"),
            (token::IDENT.to_string(), "x"),
            (token::PLUS.to_string(), "+"),
            (token::IDENT.to_string(), "y"),
            (token::SEMICOLON.to_string(), ";"),
            (token::RBRACE.to_string(), "}"),
            (token::SEMICOLON.to_string(), ";"),
            (token::LET.to_string(), "let"),
            (token::IDENT.to_string(), "result"),
            (token::ASSIGN.to_string(), "="),
            (token::IDENT.to_string(), "add"),
            (token::LPAREN.to_string(), "("),
            (token::IDENT.to_string(), "five"),
            (token::COMMA.to_string(), ","),
            (token::IDENT.to_string(), "ten"),
            (token::RPAREN.to_string(), ")"),
            (token::SEMICOLON.to_string(), ";"),
            (token::BANG.to_string(), "!"),
            (token::MINUS.to_string(), "-"),
            (token::SLASH.to_string(), "/"),
            (token::ASTERISK.to_string(), "*"),
            (token::INT.to_string(), "5"),
            (token::SEMICOLON.to_string(), ";"),
            (token::INT.to_string(), "5"),
            (token::LT.to_string(), "<"),
            (token::INT.to_string(), "10"),
            (token::GT.to_string(), ">"),
            (token::INT.to_string(), "5"),
            (token::SEMICOLON.to_string(), ";"),
            (token::IF.to_string(), "if"),
            (token::LPAREN.to_string(), "("),
            (token::INT.to_string(), "5"),
            (token::LT.to_string(), "<"),
            (token::INT.to_string(), "10"),
            (token::RPAREN.to_string(), ")"),
            (token::LBRACE.to_string(), "{"),
            (token::RETURN.to_string(), "return"),
            (token::TRUE.to_string(), "true"),
            (token::SEMICOLON.to_string(), ";"),
            (token::RBRACE.to_string(), "}"),
            (token::ELSE.to_string(), "else"),
            (token::LBRACE.to_string(), "{"),
            (token::RETURN.to_string(), "return"),
            (token::FALSE.to_string(), "false"),
            (token::SEMICOLON.to_string(), ";"),
            (token::RBRACE.to_string(), "}"),
            (token::INT.to_string(), "10"),
            (token::EQ.to_string(), "=="),
            (token::INT.to_string(), "10"),
            (token::SEMICOLON.to_string(), ";"),
            (token::INT.to_string(), "10"),
            (token::NOT_EQ.to_string(), "!="),
            (token::INT.to_string(), "9"),
            (token::SEMICOLON.to_string(), ";"),
            (token::ILLEGAL.to_string(), ":"),
            (token::EOF.to_string(), ""),
        ];

        let mut l = Lexer::new(input.to_string());

        for (expected_type, expected_literal) in tests.iter() {
            let tok = l.next_token();

            assert_eq!(
                tok.token_type, *expected_type,
                "wrong token type {} expected {}",
                tok.token_type, expected_type
            );
            assert_eq!(
                tok.literal, *expected_literal,
                "wrong literal {} expected {}",
                tok.literal, expected_literal
            );
        }
    }
}
