use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Clone, Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    // Other types of statements...
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    BlockStatement(BlockStatement),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    // Other types of expressions...
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::IntegerLiteral(lit) => lit.token_literal(),
            Expression::PrefixExpression(expr) => expr.token_literal(),
            Expression::InfixExpression(expr) => expr.token_literal(),
            Expression::Boolean(boolean) => boolean.token_literal(),
            Expression::IfExpression(expr) => expr.token_literal(),
            Expression::BlockStatement(stmt) => stmt.token_literal(),
            Expression::FunctionLiteral(func) => func.token_literal(),
            Expression::CallExpression(expr) => expr.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.string(),
            Expression::IntegerLiteral(lit) => lit.string(),
            Expression::PrefixExpression(expr) => expr.string(),
            Expression::InfixExpression(expr) => expr.string(),
            Expression::Boolean(boolean) => boolean.string(),
            Expression::IfExpression(expr) => expr.string(),
            Expression::BlockStatement(stmt) => stmt.string(),
            Expression::FunctionLiteral(func) => func.string(),
            Expression::CallExpression(expr) => expr.string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(stmt) => stmt.token_literal(),
            Statement::ReturnStatement(stmt) => stmt.token_literal(),
            Statement::ExpressionStatement(stmt) => stmt.token_literal(),
            // Handle other cases as needed
            // _ => unimplemented!(),
        }
    }

    fn string(&self) -> String {
        match self {
            Statement::LetStatement(stmt) => stmt.string(),
            Statement::ReturnStatement(stmt) => stmt.string(),
            Statement::ExpressionStatement(stmt) => stmt.string(),
            // Handle other cases as needed
            // _ => unimplemented!(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();

        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }

        out
    }
}

#[derive(Clone, Debug)]
pub struct LetStatement {
    pub token: token::Token, // the token::LET token
    pub name: Identifier,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(&self.name.string());
        out.push_str(" = ");

        if let Expression::Identifier(_) = self.value {
            out.push_str(&self.value.string());
        }

        out.push_str(";");

        out
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: token::Token, // the token::IDENT token
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub token: token::Token, // the token::RETURN token
    pub return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");

        if let Expression::Identifier(_) = self.return_value {
            out.push_str(&self.return_value.string());
        }

        out.push_str(";");

        out
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub token: token::Token, // the first token of the expression
    pub expression: Option<Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        if let Some(expr) = &self.expression {
            expr.string()
        } else {
            "".to_string()
        }
    }
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: token::Token, // The prefix token, e.g. !
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push('(');
        out.push_str(&self.operator);
        out.push_str(&self.right.string());
        out.push(')');

        out
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub token: token::Token, // The operator token, e.g. +
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push('(');
        out.push_str(&self.left.string());
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        out.push_str(&self.right.string());
        out.push(')');

        out
    }
}

#[derive(Clone, Debug)]
pub struct Boolean {
    pub token: token::Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub struct IfExpression {
    pub token: token::Token, // The 'if' token
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str("if");
        out.push_str(&self.condition.string());
        out.push_str(" ");
        out.push_str(&self.consequence.string());

        if let Some(alt) = &self.alternative {
            out.push_str("else ");
            out.push_str(&alt.string());
        }

        out
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub token: token::Token, // The { token
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }

        out
    }
}

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
    pub token: token::Token, // The 'fn' token
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        let params: Vec<String> = self.parameters.iter().map(|p| p.string()).collect();

        out.push_str(&self.token_literal());
        out.push('(');
        out.push_str(&params.join(", "));
        out.push_str(") ");
        out.push_str(&self.body.string());

        out
    }
}

#[derive(Clone, Debug)]
pub struct CallExpression {
    pub token: token::Token, // The '(' token
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        let args: Vec<String> = self.arguments.iter().map(|a| a.string()).collect();

        out.push_str(&self.function.string());
        out.push('(');
        out.push_str(&args.join(", "));
        out.push(')');

        out
    }
}

#[cfg(test)]
mod tests {
    use crate::token;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: token::Token {
                    token_type: token::LET.to_string(),
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: token::Token {
                        token_type: token::IDENT.to_string(),
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: token::Token {
                        token_type: token::IDENT.to_string(),
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }),
            })],
        };

        assert_eq!(program.string(), "let myVar = anotherVar;");
    }
}
