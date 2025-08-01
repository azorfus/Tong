#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Num, Add, Sub, Div, Mul, Dot, True, Pub,
    Opt, Cpt, Ocl, Ccl, Scln, Equ, False, Eof,
    Eqv, Gre, Les, Geq, Leq, Break, Str, Mod,
    Loop, If, Elif, Else, Func, Slash, Return,
    Iden, Qt, And, Or, Let, NewLine, Com, Import
}

#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub value: String,
    pub line_num: u32,
}

pub fn lex(file_buffer: &str, pos: &mut usize, line_number: &mut u32) -> Option<Token> {
    let chars: Vec<char> = file_buffer.chars().collect();

    while *pos < chars.len() {
        if chars[*pos] == '#' {
            while *pos < chars.len() && chars[*pos] != '\n' {
                *pos += 1;
            }
            continue;
        }

        if chars[*pos].is_whitespace() {
            if chars[*pos] == '\n' {
                *line_number += 1;
            }
            *pos += 1;
            continue;
        }

        let tok = match chars[*pos] {
            '+' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Add, value: "+".to_string(), line_num: *line_number})
            }
            '-' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Sub, value: "-".to_string(), line_num: *line_number})
            }
            '*' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Mul, value: "*".to_string(), line_num: *line_number})
            }
            '/' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Div, value: "/".to_string(), line_num: *line_number})
            }
            '%' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Mod, value: "%".to_string(), line_num: *line_number})
            }
            '(' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Opt, value: "(".to_string(), line_num: *line_number})
            }
            ')' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Cpt, value: ")".to_string(), line_num: *line_number})
            }
            '{' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Ocl, value: "{".to_string(), line_num: *line_number})
            }
            '}' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Ccl, value: "}".to_string(), line_num: *line_number})
            }
            ',' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Com, value: ",".to_string(), line_num: *line_number})
            }
            ';' => {
                *pos += 1;
                Some(Token { ttype: TokenType::Scln, value: ";".to_string(), line_num: *line_number})
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
                return Some(Token { ttype: TokenType::Str, value: literal, line_num: *line_number});
            }
            '=' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Eqv, value: "==".to_string(), line_num: *line_number})
                } else {
                    Some(Token { ttype: TokenType::Equ, value: "=".to_string(), line_num: *line_number})
                }
            }
            '<' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Leq, value: "<=".to_string(), line_num: *line_number})
                } else {
                    Some(Token { ttype: TokenType::Les, value: "<".to_string(), line_num: *line_number})
                }
            }
            '>' => {
                *pos += 1;
                if *pos < chars.len() && chars[*pos] == '=' {
                    *pos += 1;
                    Some(Token { ttype: TokenType::Geq, value: ">=".to_string(), line_num: *line_number})
                } else {
                    Some(Token { ttype: TokenType::Gre, value: ">".to_string(), line_num: *line_number})
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
            return Some(Token { ttype: TokenType::Num, value: val, line_num: *line_number});
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
                "import" => TokenType::Import,
                "pub" => TokenType::Pub,
                "fn" => TokenType::Func,
                "and" => TokenType::And,
                "or" => TokenType::Or,
                "let" => TokenType::Let,
                _ => TokenType::Iden,
            };
            return Some(Token { ttype: token_type, value: val, line_num: *line_number});
        } else {
            eprintln!("[!] [Lexer Error] Unknown character '{}' at line {}", chars[*pos], line_number);
            return None;
        }

        *pos += 1;
    }

    Some(Token {
        ttype: TokenType::Eof,
        value: String::new(),
        line_num: *line_number})
}
