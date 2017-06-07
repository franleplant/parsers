

#[derive(Debug, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub enum TokenType {
    Oprel,
    Opmat,
    Var,
    Bool,
    Number,
    Id,
    ParOpen,
    ParClose,
    If,
    Else,
    Set,
    And,
    Or,
    Not,
    String,
    Define,
}

//TODO can this Token type be generic on the _type ?
//TODO becareful, lr0/Token is incompatible with this Token str
#[derive(Clone, Debug)]
pub struct Token {
    _type: TokenType,
    value: String,
    line: usize,
    col: usize,
}

impl Token {
    pub fn new(_type: TokenType, line: usize, col: usize) -> Token {
        Token {
            _type: _type,
            line: line,
            col: col,
            value: String::new(),
        }
    }

    pub fn get_type(&self) -> String {
        let type_str = format!("{:?}", self._type);
        type_str
    }

    ////pub fn from_type(_type: Symbol) -> Token {
        ////Token {
            ////_type: _type,
        ////}
    ////}

    ////pub fn from_str(s: &str) -> Vec<Token> {
        ////s.clone().to_string().chars()
            ////.map(|c| c.to_string())
            ////.map(|s| Token::from_type(s))
            ////.collect()
    ////}

}


#[derive(Debug)]
pub struct Lexer {
    source: String,
    index: usize,
    line: usize,
    chars: Vec<char>,
    len: usize,
    line_offset: usize,
    separator_expected: bool,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        let mut chars = source.chars().collect::<Vec<char>>();
        // Assure that the end of a file is a line feed
        chars.push('\n');
        let len = chars.len();
        Lexer {
            source: source,
            chars: chars,
            len: len,
            line: 0,
            index: 0,
            line_offset: 0,
            separator_expected: false,
        }
    }

    fn user_col(&self) -> usize {
        self.index - self.line_offset + 1
    }

    fn user_line(&self) -> usize {
        self.line + 1
    }



    pub fn get_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = vec![];

        loop {
            if self.index >= self.len {
                return Ok(tokens)
            }


            let c = self.chars[self.index];
            let line = self.user_line();
            let col = self.user_col();

            //println!("c {:?}", c);
            //println!("index {:?}", self.index);
            //println!("tokens {:?}", tokens);

            if c == '\n' {
                self.index += 1;
                self.line += 1;
                self.line_offset = self.index;
                self.separator_expected = false;
                continue;
            }

            if c.is_whitespace() {
                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == '(' {
                tokens.push(Token::new(TokenType::ParOpen, line, col));
                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == ')' {
                tokens.push(Token::new(TokenType::ParClose, line, col));
                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == '+' || c == '*' || c == '^' || c == '-' || c == '/' {

                let mut value = String::new();
                value.push(c);
                let token = Token {
                    _type: TokenType::Opmat,
                    value: value,
                    line: line,
                    col: col,
                };

                tokens.push(token);

                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == '<' || c == '>' {
                let mut value = String::new();
                value.push(c);

                //TODO EOF error prevention
                let c_next = self.chars[self.index + 1];
                if c_next == '=' {
                    value.push(c_next);
                    self.index += 1;
                }


                let token = Token {
                    _type: TokenType::Oprel,
                    value: value,
                    line: line,
                    col: col,
                };

                tokens.push(token);

                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == '=' {
                let token = Token {
                    _type: TokenType::Oprel,
                    value: c.to_string(),
                    line: line,
                    col: col,
                };

                tokens.push(token);

                self.index += 1;
                self.separator_expected = false;
                continue;
            }

            if c == '"' {
                let mut value = String::new();
                let mut c = c;
                let mut first_delimiter = true;

                loop {
                    value.push(c);
                    self.index += 1;

                    if !first_delimiter && c == '"' {
                        break;
                    }

                    first_delimiter = false;
                    if self.index >= self.len {
                        return Err(format!("ERROR, unterminated string {} in line {} col {}", value, line, col));
                    }
                    c = self.chars[self.index];
                }

                if self.separator_expected {
                    return Err(format!("ERROR, unexpected token {} in line {} col {}", value, line, col));
                }

                self.separator_expected = true;

                let token = Token {
                    _type: TokenType::String,
                    value: value,
                    line: line,
                    col: col,
                };

                tokens.push(token);
                continue;
            }

            if c.is_alphabetic() {
                let mut value = String::new();
                let mut c = c;
                while c.is_alphabetic() {
                    value.push(c);
                    self.index += 1;
                    c = self.chars[self.index];
                }

                let _type = match value.as_str() {
                    "define" => TokenType::Define,
                    "if" => TokenType::If,
                    "true" => TokenType::Bool,
                    "false" => TokenType::Bool,
                    "set" => TokenType::Set,
                    "or" => TokenType::Or,
                    "not" => TokenType::Not,
                    "and" => TokenType::And,
                    _ => TokenType::Id,
                };

                if self.separator_expected {
                    return Err(format!("ERROR, unexpected token {} in line {} col {}", value, line, col));
                }

                self.separator_expected = true;

                let token = Token {
                    _type: _type,
                    value: value,
                    line: line,
                    col: col,
                };

                tokens.push(token);
                continue;
            }

            if c.is_numeric() {
                let mut value = String::new();
                let mut c = c;
                while c.is_numeric() {
                    value.push(c);
                    self.index += 1;
                    c = self.chars[self.index];
                }

                if self.separator_expected {
                    return Err(format!("ERROR, unexpected token {} in line {} col {}", value, line, col));
                }

                self.separator_expected = true;

                let token = Token {
                    _type: TokenType::Number,
                    value: value,
                    line: line,
                    col: col,
                };


                tokens.push(token);
                continue;
            }

            return Err(format!("ERROR, unexpected token"));
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trivial() {
        let mut lexer = Lexer::new("".to_string());
        let tokens = lexer.get_tokens().unwrap();
        assert_eq!(tokens.len(), 0);

        let mut lexer = Lexer::new("true".to_string());
        let tokens = lexer.get_tokens().unwrap();
        assert_eq!(tokens.len(), 1);

        let mut lexer = Lexer::new("()".to_string());
        let tokens = lexer.get_tokens().unwrap();
        assert_eq!(tokens.len(), 2);
    }

    #[test]
    fn lex1() {
        use std::io::prelude::*;
        use std::fs::File;
        let path = "./lisp.src";

        let mut file = File::open(path).expect("file not found");
        let mut content = String::new();
        file.read_to_string(&mut content).expect("error reading file");


        let mut lexer = Lexer::new(content.clone());
        let tokens = lexer.get_tokens().unwrap();

        println!("{:?}", content);
        for t in tokens {
            let type_str = format!("{:?}", t._type);
            println!("{:<10} {:<10} {:<10} {:<10}", type_str, t.value, t.line, t.col);
        }
    }


    #[test]
    #[should_panic]
    fn error() {
        let mut lexer = Lexer::new("asd123".to_string());
        lexer.get_tokens().map_err(|err| println!("{}", err)).unwrap();

        let mut lexer = Lexer::new("123asd".to_string());
        lexer.get_tokens().map_err(|err| println!("{}", err)).unwrap();

        let mut lexer = Lexer::new("\"asd".to_string());
        lexer.get_tokens().map_err(|err| println!("{}", err)).unwrap();

        let mut lexer = Lexer::new("\"".to_string());
        lexer.get_tokens().map_err(|err| println!("{}", err)).unwrap();

        let mut lexer = Lexer::new("123\"asd".to_string());
        lexer.get_tokens().map_err(|err| println!("{}", err)).unwrap();
    }
}
