use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum ASTNode {

    Eof,

    Number(f64),

    Identifier(String),

    ImportNode(String),

    StrLiteral(String),

    BreakNode,

    BoolNode(bool),

    ReturnNode(Option<Box<ASTNode>>),

    BinOpNode {
        op: String,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },

    VarDecNode {
        name: String,
        value: Box<ASTNode>,
    },

    AssignNode {
        name: String,
        value: Box<ASTNode>,
    },

    IfElseNode {
        condition: Box<ASTNode>,
        then_branch: Vec<ASTNode>,
        elif_branch: Vec<(Box<ASTNode>, Vec<ASTNode>)>,
        else_branch: Option<Vec<ASTNode>>,
    },

    LoopNode {
        condition: Box<ASTNode>,
        block: Vec<ASTNode>,
    },

    FuncCall {
        name: String,
        arguments: Vec<ASTNode>,
    },

    FuncDef {
        name: String,
        arguments: Vec<ASTNode>,
        block: Vec<ASTNode>,
    },

}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        return Self {
            tokens,
            pos: 0,
        };
    }

    fn consume(&mut self) {
        if self.pos <= self.tokens.len() {
            self.pos += 1;
        }
    }

    fn puke(&mut self) {
        if self.pos - 1 >= 0 {
            self.pos -= 1;
        }
    }

    fn current(&self) -> Option<&Token> {
        return self.tokens.get(self.pos);
    }    

    fn shout_err(&self, desc: &str, token: Option<&Token>) {
        if let Some(tok) = token {
            eprintln!(
                "[!] [Parser Error] Error parsing at line: {} ({})",
                tok.line_num, desc
            );
        } else {
            eprintln!(
                "[!] [Parser Error] Unexpected end of input ({})",
                desc
            );
        }
    }

    pub fn is_at_end(&self) -> bool {
        matches!(self.current(), Some(Token { ttype: TokenType::Eof, .. }))
    }

    fn parse_factor(&mut self) -> Option<ASTNode> {
        let token = self.current()?; // Safely unwrap Option<&Token>

        match token.ttype {
            TokenType::Num => {
                let num = token.value.parse::<f64>().ok()?;
                self.consume();
                return Some(ASTNode::Number(num));
            }

            TokenType::Iden => { 
                self.consume();
                if self.current()?.ttype == TokenType::Opt {
                    self.puke();
                    return self.parse_func_call();
                }
                else { 
                    self.puke(); 
                    let iden = self.current()?.value.clone();
                    self.consume();
                    return Some(ASTNode::Identifier(iden));
                }
            }

            TokenType::Str => {
                let iden = token.value.clone();
                self.consume();
                return Some(ASTNode::StrLiteral(iden));
            }

            TokenType::True => {
                self.consume();
                return Some(ASTNode::BoolNode(true));
            }

            TokenType::False => {
                self.consume();
                return Some(ASTNode::BoolNode(false));
            }

            TokenType::Opt => { 
                self.consume();
                let node = self.parse_expr(false)?; 
                let next = self.current()?; 
                if next.ttype != TokenType::Cpt {
                    self.shout_err("Expected closing parenthesis after expression", Some(&next));
                    return None;
                }
                self.consume();
                return Some(node);
            }

            _ => {
                    self.shout_err("Unexpected token in factor", self.current());
                    return None;
                }
        }
    }

    fn parse_term(&mut self) -> Option<ASTNode> {
        let mut node = self.parse_factor()?;

        while let Some(token) = self.current() {
            match token.ttype {
                TokenType::Mul | TokenType::Div | TokenType::Mod => {
                    let op = token.value.clone();
                    self.consume();
                    node = ASTNode::BinOpNode {
                        op,
                        left: Box::new(node),
                        right: Box::new(self.parse_factor()?),
                    };
                }

                _ => break,
            }
        }

        return Some(node);
    }

    fn parse_arith_expr(&mut self) -> Option<ASTNode> {
        let mut node = self.parse_term()?;

        while let Some(token) = self.current() {
            match token.ttype {
                TokenType::Add | TokenType::Sub => {
                    let op = token.value.clone();
                    self.consume();
                    node = ASTNode::BinOpNode {
                        op,
                        left: Box::new(node),
                        right: Box::new(self.parse_term()?),
                    };
                }

                _ => break,
            }
        }

        return Some(node);
    }

    fn parse_comp_expr(&mut self) -> Option<ASTNode> {
        let mut node = self.parse_arith_expr()?;

        while let Some(token) = self.current() {
            match token.ttype {
                TokenType::Geq | TokenType::Leq | TokenType::Gre | TokenType::Les | TokenType::Eqv => {
                    let op = token.value.clone();
                    self.consume();
                    node = ASTNode::BinOpNode {
                        op,
                        left: Box::new(node),
                        right: Box::new(self.parse_arith_expr()?),
                    };
                }

                _ => break,
            }
        }

        return Some(node);
    }

    fn parse_logic_expr(&mut self) -> Option<ASTNode> {
        let mut node = self.parse_comp_expr()?;

        while let Some(token) = self.current() {
            match token.ttype {
                TokenType::And | TokenType::Or => {
                    let op = token.value.clone();
                    self.consume();
                    node = ASTNode::BinOpNode {
                        op,
                        left: Box::new(node),
                        right: Box::new(self.parse_comp_expr()?),
                    };
                }

                _ => break,
            }
        }

        return Some(node);
    }

    fn parse_expr(&mut self, terminate: bool) -> Option<ASTNode> {
        match self.current()?.ttype {
            
            TokenType::Iden | TokenType::Num | TokenType::Str | 
            TokenType::True | TokenType::False => {
                let mut node = self.parse_logic_expr()?;
                if terminate == true && self.current()?.ttype != TokenType::Scln {
                    self.shout_err("Expected Semicolon", self.current());
                    return None;
                } else if terminate == true && self.current()?.ttype == TokenType::Scln {
                    self.consume();
                }

                return Some(node);
            }

            _ =>    {
                        None
                    },
        }

    }

    pub fn parse_statement(&mut self) -> Option<ASTNode> {
        match self.current()?.ttype {
            TokenType::Eof => return Some(ASTNode::Eof),
            TokenType::Import => self.parse_import(),
            TokenType::Let => self.parse_var_def(),
            TokenType::Func => self.parse_func_def(),
            TokenType::If => self.parse_ifelse(),
            TokenType::Loop => self.parse_loop(),

            TokenType::Break => {
                                    self.consume(); // consume break
                                    
                                    if self.current()?.ttype != TokenType::Scln {
                                        self.shout_err("Expected Semicolon", self.current());
                                        return None;
                                    }

                                    self.consume();
                                    Some(ASTNode::BreakNode)
                                }

            TokenType::Return => {
                self.consume(); // consume return
                if let Some(node) = self.parse_expr(true) {

                    Some(ASTNode::ReturnNode(Some(Box::new(node))))

                } else {

                    if self.current()?.ttype != TokenType::Scln {
                        self.shout_err("Expected Semicolon", self.current());
                        return None;
                    }

                    self.consume();
                    Some(ASTNode::ReturnNode(None))
                }
            }

            TokenType::Iden => {

                self.consume();
                if self.current()?.ttype == TokenType::Opt {
                    self.puke();
                    let node = self.parse_func_call();

                    if self.current()?.ttype != TokenType::Scln {
                        self.shout_err("Expected Semicolon", self.current());
                        return None;
                    }

                    self.consume();
                    return node;
                }
                else if self.current()?.ttype == TokenType::Equ {
                    self.puke();
                    return self.parse_assign();
                }
                else {  
                    self.shout_err("Unexpected token in statement", self.current());
                    return None; 
                }
            }

            _ => self.parse_expr(true),
        }
    }

    fn parse_import(&mut self) -> Option<ASTNode> {
        self.consume(); // consume the import token

        if self.current()?.ttype != TokenType::Str {
            self.shout_err("Invalid Module", self.current());
            return None;
        }

        let name = self.current()?.value.clone();
        self.consume();

        return Some(ASTNode::ImportNode(name));
    }

    fn parse_block(&mut self) -> Option<Vec<ASTNode>> {

        if self.current()?.ttype != TokenType::Ocl {
            self.shout_err("Expected opening brace '{' for block", self.current());
            return None;
        }

        self.consume(); // consume {
        
        let mut statements: Vec<ASTNode> = Vec::new();

        while let Some(token) = self.current() {

            if token.ttype == TokenType::Ccl {
                break
            }

            if let Some(node) = self.parse_statement() {
                statements.push(node);
            } else {
                return None;
            }
        } 

        if self.current()?.ttype != TokenType::Ccl {
            self.shout_err("Unterminated block", self.current());
            return None; // unterminated block
        }

        if self.current()?.ttype != TokenType::Ccl {
            self.shout_err("Expected closing brace '}' for block", self.current());
            return None;
        }

        self.consume(); // Consume }

        return Some(statements);
    }

    fn parse_var_def(&mut self) -> Option<ASTNode> {
        self.consume(); // consume the 'let'

        let name = self.current()?.value.clone();
        self.consume();
        self.consume(); // consume the '='
        let value = self.parse_expr(true)?;

        let mut node = ASTNode::VarDecNode {
            name: name,
            value: Box::new(value),
        };

        return Some(node);
    }

    fn parse_func_def(&mut self) -> Option<ASTNode> { 
        self.consume(); // consume the 'fn'

        let name = self.current()?.value.clone();
        self.consume();
        let arguments = self.parse_args_def()?;  

        if let Some(block) = self.parse_block() {  

            let node = ASTNode::FuncDef {
                name,
                arguments,
                block,
            };

            return Some(node);

        }
        else {
            return None;
        }
    }

    fn parse_func_call(&mut self) -> Option<ASTNode> {
        let name = self.current()?.value.clone();
        self.consume();

        let arguments = self.parse_args_call()?;

        let node = ASTNode::FuncCall {
            name,
            arguments,
        };

        return Some(node); 
    }

    fn parse_args_def(&mut self) -> Option<Vec<ASTNode>> {
        self.consume(); // consume ( 

        if self.current()?.ttype == TokenType::Cpt {
            self.consume(); // consume ) 
            return None;
        }

        let mut arguments = Vec::new();

        while let Some(token) = self.current() {

            if token.ttype == TokenType::Cpt {
                self.consume(); // consume ) 
                return Some(arguments);
            }

            arguments.push(ASTNode::Identifier(token.value.clone()));
            self.consume(); // consume identifier 

            match self.current()?.ttype {

                TokenType::Com => {
                    self.consume(); // consume , 
                }
                TokenType::Cpt => {
                    self.consume(); // consume ) 
                    return Some(arguments);
                }

                _ => {
                        return None;
                     }
            }
        }
        None
    }

    fn parse_args_call(&mut self) -> Option<Vec<ASTNode>> {
        self.consume(); // consume (

        let mut arguments = Vec::new();

        loop {
            if self.current()?.ttype == TokenType::Cpt {
                self.consume(); // consume )
                return Some(arguments);
            }

            let node = self.parse_expr(false)?; 
            arguments.push(node);

            match self.current()?.ttype {
                TokenType::Com => {
                    self.consume(); // consume ,
                }
                TokenType::Cpt => {
                    self.consume(); // consume )
                    return Some(arguments);
                }
                _ => {
                        self.shout_err("Error parsing at Token: (Call error)", self.current());
                        return None;
                     }
            }
        }

    }

    fn parse_loop(&mut self) -> Option<ASTNode> {
        self.consume(); // consume loop identifier

        if self.current()?.ttype != TokenType::Opt {
            self.shout_err("Expected opening parenthesis after 'loop'", self.current());
            return None;
        }
        self.consume(); // consume (

        let condition = self.parse_expr(false)?;

        if self.current()?.ttype != TokenType::Cpt {
            self.shout_err("Expected closing parenthesis after loop condition", self.current());
            return None;
        }
        self.consume(); // consume )

        let block = self.parse_block()?;

        let node = ASTNode::LoopNode {
            condition: Box::new(condition),
            block,
        };

        return Some(node);

    }

    /* For reference:
        
        IfElseNode {
            condition: Box<ASTNode>,
            then_branch: Vec<ASTNode>,
            elif_branch: Vec<(Box<ASTNode>, Vec<ASTNode>)>,
            else_branch: Option<Vec<ASTNode>>,
        },
    
    */

    fn parse_ifelse(&mut self) -> Option<ASTNode> {
        self.consume(); // consume if identifier

        if self.current()?.ttype != TokenType::Opt {
            return None;
        }
        self.consume(); // consume (

        let ifcondition = self.parse_expr(false)?;
        if self.current()?.ttype != TokenType::Cpt {
            return None;
        }
        self.consume(); // consume )

        let then_branch = self.parse_block()?;

        let mut elif_branches: Vec<(Box<ASTNode>, Vec<ASTNode>)> = Vec::new();
        while self.current()?.ttype == TokenType::Elif {

            self.consume(); // consume elif identifier

            if self.current()?.ttype != TokenType::Opt {
                return None;
            }
            self.consume(); // consume (

            let elifcondition = self.parse_expr(false)?;
            if self.current()?.ttype != TokenType::Cpt {
                return None;
            }
            self.consume(); // consume )

            let ethen_branch = self.parse_block()?;

            elif_branches.push((Box::new(elifcondition), ethen_branch));
        }

        let else_branch = if self.current()?.ttype == TokenType::Else {
            
                self.consume();
                Some(self.parse_block()?)
            }
            else {
                None
            };


        return Some(ASTNode::IfElseNode {
            condition: Box::new(ifcondition),
            then_branch,
            elif_branch: elif_branches,
            else_branch,
        });

    }

    fn parse_assign(&mut self) -> Option<ASTNode> {
        let name = self.current()?.value.clone();
        self.consume();
        self.consume(); // consume =

        let value = self.parse_expr(true)?;

        let node = ASTNode::AssignNode {
            name,
            value: Box::new(value),
        };

        return Some(node);
    }

}