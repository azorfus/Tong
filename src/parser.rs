use crate::lexer::{Token, TokenType};

#[derive(Debug )]
pub enum ParserError {
    UnexpectedToken(String, u32),
    UnterminatedBlock(u32),
    ExpectedSemicolon(u32),
    ExpectedToken(String, u32),
}

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

    fn current(&self) -> Result<&Token, ParserError> {
        self.tokens.get(self.pos)
            .ok_or(ParserError::UnexpectedToken("Unexpected end of input".into(), 0))
    }    

    fn parse_factor(&mut self) -> Result<ASTNode, ParserError> {
        let token = self.current()?; // Safely unwrap Option<&Token>

        match token.ttype {
            TokenType::Num => {
                let num = token.value.parse::<f64>().unwrap_or_default();
                self.consume();
                return Ok(ASTNode::Number(num));
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
                    return Ok(ASTNode::Identifier(iden));
                }
            }

            TokenType::Str => {
                let iden = token.value.clone();
                self.consume();
                return Ok(ASTNode::StrLiteral(iden));
            }

            TokenType::True => {
                self.consume();
                return Ok(ASTNode::BoolNode(true));
            }

            TokenType::False => {
                self.consume();
                return Ok(ASTNode::BoolNode(false));
            }

            TokenType::Opt => { 
                self.consume();
                let node = self.parse_expr(false)?; 
                let next = self.current()?; 
                if next.ttype != TokenType::Cpt {
                    // self.shout_err("Expected closing parenthesis after expression", Some(&next));
                    return Err(ParserError::ExpectedToken("closing parenthesis".into(), next.line_num));
                }
                self.consume();
                return Ok(node);
            }

            _ => {
                    // self.shout_err("Unexpected token in factor", self.current());
                    return Err(ParserError::UnexpectedToken(token.value.clone(), token.line_num));
                }
        }
    }

    fn parse_term(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_factor()?;

        loop {
            let token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };
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

        return Ok(node);
    }

    fn parse_arith_expr(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_term()?;

        loop {
            let token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };
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

        return Ok(node);
    }

    fn parse_comp_expr(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_arith_expr()?;

        loop {
            let token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };
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

        return Ok(node);
    }

    fn parse_logic_expr(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = self.parse_comp_expr()?;

        loop {
            let token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };
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

        return Ok(node);
    }

    fn parse_expr(&mut self, terminate: bool) -> Result<ASTNode, ParserError> {
        match self.current()?.ttype {
            
            TokenType::Iden | TokenType::Num | TokenType::Str | 
            TokenType::True | TokenType::False => {
                let mut node = self.parse_logic_expr()?;
                if terminate == true && self.current()?.ttype != TokenType::Scln {
                    // self.shout_err("Expected Semicolon", self.current());
                    return Err(ParserError::ExpectedSemicolon(self.current()?.line_num));
                } else if terminate == true && self.current()?.ttype == TokenType::Scln {
                    self.consume();
                }

                return Ok(node);
            }

            _ =>    {
                        Err(ParserError::UnexpectedToken("Invalid start of expression".into(), self.current()?.line_num))
                    },
        }

    }

    pub fn parse_statement(&mut self) -> Result<ASTNode, ParserError> {
        match self.current()?.ttype {
            TokenType::Eof => return Ok(ASTNode::Eof),
            TokenType::Import => self.parse_import(),
            TokenType::Let => self.parse_var_def(),
            TokenType::Func => self.parse_func_def(),
            TokenType::If => self.parse_ifelse(),
            TokenType::Loop => self.parse_loop(),

            TokenType::Break => {
                                    self.consume(); // consume break
                                    
                                    if self.current()?.ttype != TokenType::Scln {
                                        // self.shout_err("Expected Semicolon", self.current());
                                        return Err(ParserError::ExpectedSemicolon(self.current()?.line_num));
                                    }

                                    self.consume();
                                    Ok(ASTNode::BreakNode)
                                }

            TokenType::Return => {
                self.consume(); // consume return
                if let Ok(node) = self.parse_expr(true) {

                    Ok(ASTNode::ReturnNode(Some(Box::new(node))))

                } else {

                    if self.current()?.ttype != TokenType::Scln {
                        // self.shout_err("Expected Semicolon", self.current());
                        return Err(ParserError::ExpectedSemicolon(self.current()?.line_num));
                    }

                    self.consume();
                    Ok(ASTNode::ReturnNode(None))
                }
            }

            TokenType::Iden => {

                self.consume();
                if self.current()?.ttype == TokenType::Opt {
                    self.puke();
                    let node = self.parse_func_call();

                    if self.current()?.ttype != TokenType::Scln {
                        // self.shout_err("Expected Semicolon", self.current());
                        return Err(ParserError::ExpectedSemicolon(self.current()?.line_num));
                    }

                    self.consume();
                    return node;
                }
                else if self.current()?.ttype == TokenType::Equ {
                    self.puke();
                    return self.parse_assign();
                }
                else {  
                    // self.shout_err("Unexpected token in statement", self.current());
                    return Err(ParserError::UnexpectedToken(self.current()?.value.clone(), self.current()?.line_num)); 
                }
            }

            _ => self.parse_expr(true),
        }
    }

    fn parse_import(&mut self) -> Result<ASTNode, ParserError> {
        self.consume(); // consume the import token

        if self.current()?.ttype != TokenType::Str {
            // self.shout_err("Invalid Module", self.current());
            return Err(ParserError::UnexpectedToken("Invalid module string".into(), self.current()?.line_num));
        }

        let name = self.current()?.value.clone();
        self.consume();

        return Ok(ASTNode::ImportNode(name));
    }

    fn parse_block(&mut self) -> Result<Vec<ASTNode>, ParserError> {

        if self.current()?.ttype != TokenType::Ocl {
            // self.shout_err("Expected opening brace '{' for block", self.current());
            return Err(ParserError::ExpectedToken("{".into(), self.current()?.line_num));
        }

        self.consume(); // consume {
        
        let mut statements: Vec<ASTNode> = Vec::new();
        
        // seems very inefficient but rust whines if I try to 
        // use a reference here so I have to clone the token
        // and then use it in the loop condition 
        
        let thetype = self.current()?.ttype.clone();
        let thenum = self.current()?.line_num;
        let theval = self.current()?.value.clone();

        loop {

            if thetype == TokenType::Ccl {
                break;
            }

            match self.parse_statement() {
                Ok(node) => statements.push(node),
                Err(_) => {
                    return Err(ParserError::UnexpectedToken(theval, thenum));
                }
            }
        } 

        if self.current()?.ttype != TokenType::Ccl {
            // self.shout_err("Unterminated block", self.current());
            return Err(ParserError::UnterminatedBlock(self.current()?.line_num)); // unterminated block
        }

        if self.current()?.ttype != TokenType::Ccl {
            // self.shout_err("Expected closing brace '}' for block", self.current());
            return Err(ParserError::ExpectedToken("}".into(), self.current()?.line_num));
        }

        self.consume(); // Consume }

        return Ok(statements);
    }

    fn parse_var_def(&mut self) -> Result<ASTNode, ParserError> {
        self.consume(); // consume the 'let'

        let name = self.current()?.value.clone();
        self.consume();
        self.consume(); // consume the '='
        let value = self.parse_expr(true)?;

        let mut node = ASTNode::VarDecNode {
            name: name,
            value: Box::new(value),
        };

        return Ok(node);
    }

    fn parse_func_def(&mut self) -> Result<ASTNode, ParserError> { 
        self.consume(); // consume the 'fn'

        let name = self.current()?.value.clone();
        self.consume();
        let arguments = self.parse_args_def()?.unwrap_or_default();

        if let Ok(block) = self.parse_block() {  
            let node = ASTNode::FuncDef {
                name,
                arguments,
                block,
            };
            return Ok(node);
        } else {
            return Err(ParserError::UnexpectedToken("Error parsing function definition".into(), self.current()?.line_num));
        }
    }

    fn parse_func_call(&mut self) -> Result<ASTNode, ParserError> {
        let name = self.current()?.value.clone();
        self.consume();

        let arguments = self.parse_args_call()?;

        let node = ASTNode::FuncCall {
            name,
            arguments,
        };

        return Ok(node); 
    }

    fn parse_args_def(&mut self) -> Result<Option<Vec<ASTNode>>, ParserError> {
        self.consume(); // consume ( 

        if self.current()?.ttype == TokenType::Cpt {
            self.consume(); // consume ) 
            return Ok(None);
        }

        let mut arguments = Vec::new();

        loop {
            let token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };

            if token.ttype == TokenType::Cpt {
                self.consume(); // consume ) 
                return Ok(Some(arguments));
            }

            arguments.push(ASTNode::Identifier(token.value.clone()));
            self.consume(); // consume identifier 

            // After consuming, get the next token for the separator check
            let sep_token = match self.current() {
                Ok(token) => token,
                Err(_) => break,
            };

            match sep_token.ttype {
                TokenType::Com => {
                    self.consume(); // consume , 
                }
                TokenType::Cpt => {
                    self.consume(); // consume ) 
                    return Ok(Some(arguments));
                }
                _ => {
                    return Err(ParserError::UnexpectedToken("Error parsing function arguments".into(), sep_token.line_num));
                }
            }
        }
        Err(ParserError::UnexpectedToken("Error parsing function arguments".into(), 0))
    }

    fn parse_args_call(&mut self) -> Result<Vec<ASTNode>, ParserError> {
        self.consume(); // consume (

        let mut arguments = Vec::new();

        loop {
            if self.current()?.ttype == TokenType::Cpt {
                self.consume(); // consume )
                return Ok(arguments);
            }

            let node = self.parse_expr(false)?; 
            arguments.push(node);

            match self.current()?.ttype {
                TokenType::Com => {
                    self.consume(); // consume ,
                }
                TokenType::Cpt => {
                    self.consume(); // consume )
                    return Ok(arguments);
                }
                _ => {
                        // self.shout_err("Error parsing at Token: (Call error)", self.current());
                        return Err(ParserError::UnexpectedToken("Error parsing function call arguments".into(), self.current()?.line_num));
                     }
            }
        }

    }

    fn parse_loop(&mut self) -> Result<ASTNode, ParserError> {
        self.consume(); // consume loop identifier

        if self.current()?.ttype != TokenType::Opt {
            // self.shout_err("Expected opening parenthesis after 'loop'", self.current());
            return Err(ParserError::ExpectedToken("(".into(), self.current()?.line_num));
        }
        self.consume(); // consume (

        let condition = self.parse_expr(false)?;

        if self.current()?.ttype != TokenType::Cpt {
            // self.shout_err("Expected closing parenthesis after loop condition", self.current());
            return Err(ParserError::ExpectedToken(")".into(), self.current()?.line_num));
        }
        self.consume(); // consume )

        let block = self.parse_block()?;

        let node = ASTNode::LoopNode {
            condition: Box::new(condition),
            block,
        };

        return Ok(node);

    }

    /* For reference:
        
        IfElseNode {
            condition: Box<ASTNode>,
            then_branch: Vec<ASTNode>,
            elif_branch: Vec<(Box<ASTNode>, Vec<ASTNode>)>,
            else_branch: Option<Vec<ASTNode>>,
        },
    
    */

    fn parse_ifelse(&mut self) -> Result<ASTNode, ParserError> {
        self.consume(); // consume if identifier

        if self.current()?.ttype != TokenType::Opt {
            return Err(ParserError::ExpectedToken("(".into(), self.current()?.line_num));
        }
        self.consume(); // consume (

        let ifcondition = self.parse_expr(false)?;
        if self.current()?.ttype != TokenType::Cpt {
            return Err(ParserError::ExpectedToken(")".into(), self.current()?.line_num));
        }
        self.consume(); // consume )

        let then_branch = self.parse_block()?;

        let mut elif_branches: Vec<(Box<ASTNode>, Vec<ASTNode>)> = Vec::new();
        while self.current()?.ttype == TokenType::Elif {

            self.consume(); // consume elif identifier

            if self.current()?.ttype != TokenType::Opt {
                return Err(ParserError::ExpectedToken("(".into(), self.current()?.line_num));
            }
            self.consume(); // consume (

            let elifcondition = self.parse_expr(false)?;
            if self.current()?.ttype != TokenType::Cpt {
                return Err(ParserError::ExpectedToken(")".into(), self.current()?.line_num));
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


        return Ok(ASTNode::IfElseNode {
            condition: Box::new(ifcondition),
            then_branch,
            elif_branch: elif_branches,
            else_branch,
        });

    }

    fn parse_assign(&mut self) -> Result<ASTNode, ParserError> {
        let name = self.current()?.value.clone();
        self.consume();
        self.consume(); // consume =

        let value = self.parse_expr(true)?;

        let node = ASTNode::AssignNode {
            name,
            value: Box::new(value),
        };

        return Ok(node);
    }

    pub fn is_at_end(&self) -> bool {
        match self.current() {
            Ok(token) => token.ttype == TokenType::Eof,
            Err(_) => true,
        }
    }

}