#[derive(Debug, PartialEq)]

pub enum TokenType {
    // Literals & Constants
    Num,     Str,     True,    False,   Iden,
    Char,

    // Identifiers
    Ui8,     Ui16,    Ui32,    Ui64,    Ii64, 
    Bool,    Ii8,     Ii16,    Ii32,   
    Fi32,    Tensor,  Const,

    // Operators (Arithmetic & Assignment)
    Add,     Sub,     Mul,     Div,     Mod,
    Equ,     Dot,     

    // Comparison & Logical Operators
    Eqv,     Gre,     Les,     Geq,     Leq,
    And,     Or,      Bor,     Band,    Lsh,
    Rsh,     As, 

    // Punctuation & Delimiters
    Osq,     Csq,     Opt,     Cpt,     Ocl,
    Ccl,     Scln,    Com,     Qt,

    // Keywords
    Let,     If,      Elif,    Else,    Func,
    Return,  Break,   Loop,	   

    // Miscellaneous / Structural
    Slash,   NewLine, Eof,
}

#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub value: String,
}

pub fn lex(file_buffer: &str, pos: &mut usize) -> Option<Token> {
    let chars: Vec<char> = file_buffer.chars().collect();

    while *pos < chars.len() {
        if chars[*pos] == '#' {
            while *pos < chars.len() && chars[*pos] != '\n' {
                *pos += 1;
            }
            continue;
        }

        if chars[*pos].is_whitespace() {
            *pos += 1;
            continue;
        }

        let tok = match chars[*pos] {
            '+' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Add, value: "+".to_string() })
            }
            '-' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Sub, value: "-".to_string() })
            }
            '*' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Mul, value: "*".to_string() })
            }
            '/' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Div, value: "/".to_string() })
            }
            '%' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Mod, value: "%".to_string() })
            }
            '(' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Opt, value: "(".to_string() })
            }
            ')' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Cpt, value: ")".to_string() })
            }
            '{' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Ocl, value: "{".to_string() })
            }
            '}' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Ccl, value: "}".to_string() })
            }
            '[' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Osq, value: "[".to_string() })
            }
            ']' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Csq, value: "]".to_string() })
            }
            ',' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Com, value: ",".to_string() })
            }
            ';' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Scln, value: ";".to_string() })
            }
            '\"' => {
                *pos += 1;
                let mut literal = String::new();
                while *pos < chars.len() && chars[*pos] != '\"' {
                    if chars[*pos] == '\\' {
                        *pos += 1;
                        if *pos >= chars.len() {
                            return None;
                        }
                        match chars[*pos] {
                            '\"' => {
                                *pos += 1;
                                literal.push('\"');
                            }
                            'n' => {
                                *pos += 1;
                                literal.push('\n');
                            }
                            '\\' => {
                                *pos += 1;
                                literal.push('\\');
                            }
                            _ => return None,
                        }
                    } else {
                        literal.push(chars[*pos]);
                        *pos += 1;
                    }
                }
                *pos += 1;
                return Some(Token { ttype: TokenType::Str, value: literal });
            }
            '=' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Eqv, value: "==".to_string() })
                } else {
                    Some(Token { ttype: TokenType::Equ, value: "=".to_string() })
                }
            }
            '<' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Leq, value: "<=".to_string() })
                } else {
                    Some(Token { ttype: TokenType::Les, value: "<".to_string() })
                }
            }
            '>' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Geq, value: ">=".to_string() })
                } else {
                    Some(Token { ttype: TokenType::Gre, value: ">".to_string() })
                }
            }
            _ => None,
        };

        if let Some(tok) = tok {
            return Some(tok);
        }

        if chars[*pos].is_ascii_digit() {
            let mut val = String::new();
            let mut float = false;
            while *pos < chars.len() && (chars[*pos].is_ascii_digit() || chars[*pos] == '.') {
                if chars[*pos] == '.' {
                    if float {
                        return None;
                    }
                    float = true;
                }
                val.push(chars[*pos]);
                *pos += 1;
            }
            return Some(Token { ttype: TokenType::Num, value: val });
        } else if chars[*pos].is_ascii_alphabetic() || chars[*pos] == '_' {
            let mut val = String::new();
            val.push(chars[*pos]);
            *pos += 1;
            while *pos < chars.len() && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_') {
                val.push(chars[*pos]);
                *pos += 1;
            }
            let token_type = match val.as_str() {
                "loop" => TokenType::Loop,
                "if" => TokenType::If,
                "elif" => TokenType::Elif,
                "else" => TokenType::Else,
                "true" => TokenType::True,
                "false" => TokenType::False,
                "break" => TokenType::Break,
                "return" => TokenType::Return,
                "fn" => TokenType::Func,
                "and" => TokenType::And,
                "or" => TokenType::Or,
                "let" => TokenType::Let,
                "i8" => TokenType::Ii8,
                "i16" => TokenType::Ii16,
                "i32" => TokenType::Ii32,
                "i64" => TokenType::Ii64,
                "f32" => TokenType::Fi32,
                "const" => TokenType::Const,
                "tensor" => TokenType::Tensor,
                _ => TokenType::Iden,
            };
            return Some(Token { ttype: token_type, value: val });
        }

        *pos += 1;
    }

    Some(Token {
        ttype: TokenType::Eof,
        value: String::new(),
    })
}

