use std::collections::HashMap;

use crate::ast;
use crate::lexer;
use crate::token;

#[derive(Clone)]
struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<String, PrefixParseFn>,
    infix_parse_fns: HashMap<String, InfixParseFn>,
}

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;

pub enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

pub fn get_precedences() -> HashMap<String, i32> {
    let mut precedences = HashMap::new();
    precedences.insert(token::EQ.into(), Precedence::Equals as i32);
    precedences.insert(token::NOT_EQ.into(), Precedence::Equals as i32);
    precedences.insert(token::LT.into(), Precedence::LessGreater as i32);
    precedences.insert(token::GT.into(), Precedence::LessGreater as i32);
    precedences.insert(token::PLUS.into(), Precedence::Sum as i32);
    precedences.insert(token::MINUS.into(), Precedence::Sum as i32);
    precedences.insert(token::SLASH.into(), Precedence::Product as i32);
    precedences.insert(token::ASTERISK.into(), Precedence::Product as i32);

    precedences
}

impl Parser {
    fn new(l: lexer::Lexer) -> Self {
        let mut p = Parser {
            l,
            cur_token: token::Token {
                token_type: String::new(),
                literal: String::new(),
            },
            peek_token: token::Token {
                token_type: String::new(),
                literal: String::new(),
            },
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(token::IDENT.to_string(), Parser::parse_identifier);
        p.register_prefix(token::INT.to_string(), Parser::parse_integer_literal);
        p.register_prefix(token::BANG.to_string(), Parser::parse_prefix_expression);
        p.register_prefix(token::MINUS.to_string(), Parser::parse_prefix_expression);

        p.register_infix(token::PLUS.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::MINUS.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::SLASH.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::ASTERISK.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::EQ.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::NOT_EQ.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::LT.to_string(), Parser::parse_infix_expression);
        p.register_infix(token::GT.to_string(), Parser::parse_infix_expression);

        p.next_token();
        p.next_token();
        p
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: String) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(p: &mut Parser) -> Option<ast::Program> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while !p.cur_token_is(token::EOF) {
            if let Some(stmt) = Parser::parse_statement(p) {
                program.statements.push(stmt);
            }
            p.next_token();
        }

        Some(program)
    }

    fn parse_statement(p: &mut Parser) -> Option<ast::Statement> {
        match p.cur_token.token_type.as_str() {
            token::LET => Parser::parse_let_statement(p),
            token::RETURN => Parser::parse_return_statement(p),
            _ => Parser::parse_expression_statement(p),
        }
    }

    fn parse_let_statement(p: &mut Parser) -> Option<ast::Statement> {
        let mut stmt = ast::LetStatement {
            token: p.cur_token.clone(),
            name: ast::Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            },
            value: ast::Expression::Identifier(ast::Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            }),
        };

        if !p.expect_peek(token::IDENT.into()) {
            return None;
        }

        stmt.name = ast::Identifier {
            token: p.cur_token.clone(),
            value: p.cur_token.literal.clone(),
        };

        if !p.expect_peek(token::ASSIGN.into()) {
            return None;
        }

        while !p.cur_token_is(token::SEMICOLON) {
            p.next_token();
        }

        Some(ast::Statement::LetStatement(stmt))
    }

    fn cur_token_is(&self, t: &str) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: &str) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: String) -> bool {
        if self.peek_token_is(t.as_str()) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn parse_return_statement(p: &mut Parser) -> Option<ast::Statement> {
        let stmt = ast::ReturnStatement {
            token: p.cur_token.clone(),
            return_value: ast::Expression::Identifier(ast::Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            }),
        };

        p.next_token();

        while !p.cur_token_is(token::SEMICOLON) {
            p.next_token();
        }

        Some(ast::Statement::ReturnStatement(stmt))
    }

    fn register_prefix(&mut self, token_type: String, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: String, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn parse_expression_statement(p: &mut Parser) -> Option<ast::Statement> {
        let stmt = ast::ExpressionStatement {
            token: p.cur_token.clone(),
            expression: Parser::parse_expression(p, Precedence::Lowest as i32),
        };

        if p.peek_token_is(token::SEMICOLON) {
            p.next_token();
        }

        Some(ast::Statement::ExpressionStatement(stmt))
    }

    fn no_prefix_parse_fn_error(p: &mut Parser, t: String) {
        let msg = format!("no prefix parse function for {} found", t);
        p.errors.push(msg);
    }

    fn parse_expression(p: &mut Parser, precedence: i32) -> Option<ast::Expression> {
        let prefix = p.prefix_parse_fns.get(&p.cur_token.token_type);
        if prefix.is_none() {
            Parser::no_prefix_parse_fn_error(p, p.cur_token.token_type.clone());
            return None;
        }
        let mut left_exp = prefix.unwrap()(p);

        while !p.peek_token_is(token::SEMICOLON) && precedence < Parser::peek_precedence(p) {
            let cloned_p = p.clone();

            let infix = cloned_p.infix_parse_fns.get(&p.peek_token.token_type);
            if infix.is_none() {
                return left_exp;
            }
            p.next_token();
            left_exp = infix.unwrap()(p, left_exp.unwrap());
        }

        left_exp
    }

    fn parse_identifier(p: &mut Parser) -> Option<ast::Expression> {
        Some(ast::Expression::Identifier(ast::Identifier {
            token: p.cur_token.clone(),
            value: p.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(p: &mut Parser) -> Option<ast::Expression> {
        let value = p.cur_token.literal.parse::<i64>().unwrap();
        let lit = ast::IntegerLiteral {
            token: p.cur_token.clone(),
            value,
        };
        Some(ast::Expression::IntegerLiteral(lit))
    }

    fn parse_prefix_expression(p: &mut Parser) -> Option<ast::Expression> {
        let mut expression = ast::PrefixExpression {
            token: p.cur_token.clone(),
            operator: p.cur_token.literal.clone(),
            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            })),
        };

        p.next_token();

        expression.right =
            Box::new(Parser::parse_expression(p, Precedence::Prefix as i32).unwrap());

        Some(ast::Expression::PrefixExpression(expression))
    }

    fn peek_precedence(p: &mut Parser) -> i32 {
        let precedences = get_precedences();

        let precedence = precedences.get(&p.peek_token.token_type);
        if precedence.is_some() {
            return precedence.unwrap().clone();
        }

        Precedence::Lowest as i32
    }

    fn cur_precedence(p: &mut Parser) -> i32 {
        let precedences = get_precedences();

        let precedence = precedences.get(&p.cur_token.token_type);
        if precedence.is_some() {
            return precedence.unwrap().clone();
        }

        Precedence::Lowest as i32
    }

    fn parse_infix_expression(p: &mut Parser, left: ast::Expression) -> Option<ast::Expression> {
        let mut expression = ast::InfixExpression {
            token: p.cur_token.clone(),
            operator: p.cur_token.literal.clone(),
            left: Box::new(left),
            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            })),
        };

        let precedence = Parser::cur_precedence(p);
        p.next_token();
        expression.right = Box::new(Parser::parse_expression(p, precedence).unwrap());

        Some(ast::Expression::InfixExpression(expression))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = Parser::parse_program(&mut p);
        check_parser_errors(&p);

        if program.is_none() {
            panic!("ParseProgram() returned None");
        }

        if program.as_ref().unwrap().statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.unwrap().statements.len()
            );
        }

        let tests = vec!["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            let stmt: Statement = program.as_ref().unwrap().statements[i].clone();
            test_let_statement(stmt, tt);
        }
    }

    fn test_let_statement(s: Statement, name: &str) -> bool {
        match s {
            Statement::LetStatement(let_stmt) => {
                if let_stmt.token_literal() != "let" {
                    return false;
                }

                let name_value = match &let_stmt.name {
                    Identifier { value, .. } => value,
                };
                if name_value != name {
                    return false;
                }

                let name_literal = let_stmt.name.token_literal();
                if name_literal != name {
                    return false;
                }

                true
            }
            _ => false,
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());
        for msg in errors {
            println!("parser error: {}", msg);
        }
        panic!();
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = Parser::parse_program(&mut p);
        check_parser_errors(&p);

        if program.is_none() {
            panic!("ParseProgram() returned None");
        }

        if program.as_ref().unwrap().statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.unwrap().statements.len()
            );
        }

        for stmt in program.unwrap().statements {
            match stmt {
                Statement::ReturnStatement(return_stmt) => {
                    if return_stmt.token_literal() != "return" {
                        panic!(
                            "return_stmt.token_literal not 'return'. got={}",
                            return_stmt.token_literal()
                        );
                    }
                }
                _ => panic!("stmt not ReturnStatement. got={:?}", stmt.token_literal()),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = Parser::parse_program(&mut p);
        check_parser_errors(&p);

        if program.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "program does not contain 1 statement. got={}",
                program.unwrap().statements.len()
            );
        }

        let stmt = program.unwrap().statements[0].clone();
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                let ident = match expr_stmt.expression {
                    Some(Expression::Identifier(ident)) => ident,
                    _ => panic!(
                        "exp not Identifier. got={:?}",
                        expr_stmt.expression.unwrap().string()
                    ),
                };

                if ident.value != "foobar" {
                    panic!("ident.value not {}. got={}", "foobar", ident.value);
                }

                if ident.token_literal() != "foobar" {
                    panic!(
                        "ident.token_literal not {}. got={}",
                        "foobar",
                        ident.token_literal()
                    );
                }
            }
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt.string()),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = Parser::parse_program(&mut p);
        check_parser_errors(&p);

        if program.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "program does not contain 1 statement. got={}",
                program.unwrap().statements.len()
            );
        }

        let stmt = program.unwrap().statements[0].clone();
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                let literal = match expr_stmt.expression {
                    Some(Expression::IntegerLiteral(literal)) => literal,
                    _ => panic!(
                        "exp not IntegerLiteral. got={:?}",
                        expr_stmt.expression.unwrap().string()
                    ),
                };

                if literal.value != 5 {
                    panic!("literal.value not {}. got={}", 5, literal.value);
                }

                if literal.token_literal() != "5" {
                    panic!(
                        "literal.token_literal not {}. got={}",
                        "5",
                        literal.token_literal()
                    );
                }
            }
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt.string()),
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![("!5", "!", 5), ("-15", "-", 15)];

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = Parser::parse_program(&mut p);
            check_parser_errors(&p);

            if program.as_ref().unwrap().statements.len() != 1 {
                panic!(
                    "program does not contain 1 statement. got={}",
                    program.unwrap().statements.len()
                );
            }

            let stmt = program.unwrap().statements[0].clone();
            match stmt {
                Statement::ExpressionStatement(expr_stmt) => {
                    let exp = match expr_stmt.expression {
                        Some(Expression::PrefixExpression(exp)) => exp,
                        _ => panic!(
                            "exp not PrefixExpression. got={:?}",
                            expr_stmt.expression.unwrap().string()
                        ),
                    };

                    if exp.operator != tt.1 {
                        panic!("exp.operator is not {}. got={}", tt.1, exp.operator);
                    }

                    if !test_integer_literal(Some(exp.right), tt.2) {
                        return;
                    }
                }
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt.string()),
            }
        }
    }

    fn test_integer_literal(il: Option<Box<Expression>>, value: i64) -> bool {
        let integer_literal = match il {
            Some(il) => match *il {
                Expression::IntegerLiteral(literal) => literal,
                _ => panic!("il not IntegerLiteral. got={:?}", il),
            },
            None => panic!("il not IntegerLiteral. got=None"),
        };

        if integer_literal.value != value {
            panic!(
                "integer_literal.value not {}. got={}",
                value, integer_literal.value
            );
        }

        if integer_literal.token_literal() != format!("{}", value) {
            panic!(
                "integer_literal.token_literal not {}. got={}",
                value,
                integer_literal.token_literal()
            );
        }

        true
    }

    #[test]
    fn test_parsing_infix_expression() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for tt in infix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = Parser::parse_program(&mut p);
            check_parser_errors(&p);

            if program.as_ref().unwrap().statements.len() != 1 {
                panic!(
                    "program does not contain 1 statement. got={}",
                    program.unwrap().statements.len()
                );
            }

            let stmt = program.unwrap().statements[0].clone();
            match stmt {
                Statement::ExpressionStatement(expr_stmt) => {
                    let exp = match expr_stmt.expression {
                        Some(Expression::InfixExpression(exp)) => exp,
                        _ => panic!(
                            "exp not InfixExpression. got={:?}",
                            expr_stmt.expression.unwrap().string()
                        ),
                    };

                    if !test_integer_literal(Some(exp.left), tt.1) {
                        return;
                    }

                    if exp.operator != tt.2 {
                        panic!("exp.operator is not {}. got={}", tt.2, exp.operator);
                    }

                    if !test_integer_literal(Some(exp.right), tt.3) {
                        return;
                    }
                }
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt.string()),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for tt in tests {
            let l = lexer::Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = Parser::parse_program(&mut p);
            check_parser_errors(&p);

            let actual = program.as_ref().unwrap().string();

            if actual != tt.1.to_string() {
                panic!("expected={}, got={}", tt.1, actual);
            }
        }
    }
}
