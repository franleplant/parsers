
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
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        let chars = source.chars().collect::<Vec<char>>();
        let len = chars.len();
        Lexer {
            source: source,
            chars: chars,
            len: len,
            line: 0,
            index: 0,
            line_offset: 0,
            separator_expected: false,
            tokens: vec![],
        }

    }

    fn user_col(&self) -> usize {
        self.index - self.line_offset + 1
    }

    fn user_line(&self) -> usize {
        self.line + 1
    }

    fn is_separator(c: char) -> bool {
        match c {
            '\n' | '(' | ')' | '+' | '*' | '^' | '-' | '/' | '<' | '>' | '=' => true,
            _ if c.is_whitespace() => true,
            _ => false,
        }
    }

    fn get_char(&self) -> (char, Option<char>) {
        assert!(self.index < self.len,
                "get_char: trying to access out of bound char");
        let c = self.chars[self.index];
        let c_next = {
            let next_index = self.index + 1;
            if next_index < self.len {
                Some(self.chars[next_index])
            } else {
                None
            }
        };

        (c, c_next)
    }


    pub fn tokens_as_tuples(&self) -> Vec<(String, String, usize, usize)> {
        self.tokens
            .iter()
            .map(|t| {
                     let type_str = format!("{:?}", t._type);
                     (type_str, t.value.clone(), t.line, t.col)
                 })
            .collect()
    }

    pub fn get_tokens(&mut self) -> Result<&Vec<Token>, String> {
        loop {
            if self.index >= self.len {
                return Ok(&self.tokens);
            }

            let (c, _) = self.get_char();
            let line = self.user_line();
            let col = self.user_col();
            let mut value = String::new();
            let mut token_type: Option<TokenType> = None;


            //println!("c {:?}", c);
            //println!("index {:?}", self.index);
            //println!("tokens {:?}", self.tokens);
            match c {
                '\n' => {
                    self.line += 1;
                    self.line_offset = self.index + 1;
                }
                _ if c.is_whitespace() => {}

                '(' => token_type = Some(TokenType::ParOpen),
                ')' => token_type = Some(TokenType::ParClose),

                '+' | '*' | '^' | '-' | '/' => {
                    token_type = Some(TokenType::Opmat);
                    value.push(c);
                }
                '<' | '>' => {
                    token_type = Some(TokenType::Oprel);
                    value.push(c);

                    // EOF error prevention
                    let next_index = self.index + 1;
                    if next_index >= self.len {
                        return Err(format!("ERROR, unexpected end of file in line {} col {}",
                                           line,
                                           col));
                    }
                    let c_next = self.chars[next_index];
                    if c_next == '=' {
                        value.push(c_next);
                        self.index += 1;
                    }


                }
                '=' => {
                    token_type = Some(TokenType::Oprel);
                    value.push(c);
                }

                '"' => {
                    token_type = Some(TokenType::String);
                    let mut first_delimiter = true;


                    loop {
                        let (c, _) = self.get_char();
                        value.push(c);

                        if !first_delimiter && c == '"' {
                            break;
                        }
                        first_delimiter = false;

                        self.index += 1;
                        if self.index >= self.len {
                            return Err(format!("ERROR, unterminated string {} in line {} col {}",
                                               value,
                                               line,
                                               col));
                        }
                    }
                }
                _ if c.is_alphabetic() => {
                    loop {
                        let (c, c_next) = self.get_char();
                        value.push(c);
                        if let Some(next) = c_next {
                            if !next.is_alphabetic() {
                                break;
                            }
                            self.index += 1;
                        } else {
                            break;
                        }

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

                    token_type = Some(_type);
                }
                _ if c.is_numeric() => {
                    token_type = Some(TokenType::Number);
                    loop {
                        let (c, c_next) = self.get_char();
                        value.push(c);
                        if let Some(next) = c_next {
                            if !next.is_alphabetic() {
                                break;
                            }
                            self.index += 1;
                        } else {
                            break;
                        }

                    }

                }
                _ => {
                    return Err(format!("ERROR, unexpected token"));
                }
            }

            if self.separator_expected && !Lexer::is_separator(c) {
                return Err(format!("ERROR, separator expected in {} in line {} col {}",
                                   value,
                                   line,
                                   col));
            }

            self.separator_expected = !Lexer::is_separator(c);

            self.index += 1;

            if let Some(_type) = token_type {
                self.tokens
                    .push(Token {
                              _type: _type,
                              value: value,
                              line: line,
                              col: col,
                          });
            }
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
        file.read_to_string(&mut content)
            .expect("error reading file");


        let mut lexer = Lexer::new(content.clone());
        {
            lexer.get_tokens().unwrap();
        }

        println!("{:?}", content);

        let token_tuples = lexer.tokens_as_tuples();
        for t in &token_tuples {
            println!("{:<10} {:<10} {:<10} {:<10}", t.0, t.1, t.2, t.3);
        }

        let expected_token_tuples: Vec<(String, String, usize, usize)> =
            vec![("ParOpen", "", 1, 1),
                 ("Define", "define", 1, 2),
                 ("ParOpen", "", 1, 9),
                 ("Id", "myfn", 1, 10),
                 ("Id", "x", 1, 15),
                 ("Id", "y", 1, 17),
                 ("ParClose", "", 1, 18),
                 ("ParOpen", "", 1, 20),
                 ("ParOpen", "", 2, 3),
                 ("If", "if", 2, 4),
                 ("ParOpen", "", 2, 7),
                 ("Oprel", ">", 2, 8),
                 ("Id", "x", 2, 10),
                 ("Id", "y", 2, 12),
                 ("ParClose", "", 2, 13),
                 ("Id", "x", 3, 5),
                 ("Id", "y", 4, 5),
                 ("ParClose", "", 4, 6),
                 ("ParClose", "", 5, 1),
                 ("ParClose", "", 5, 2),
                 ("ParOpen", "", 7, 1),
                 ("Id", "myfn", 7, 2),
                 ("Number", "2", 7, 7),
                 ("Number", "4", 7, 9),
                 ("ParClose", "", 7, 10),
                 ("ParOpen", "", 9, 1),
                 ("And", "and", 9, 2),
                 ("Bool", "true", 9, 6),
                 ("Bool", "false", 9, 11),
                 ("ParClose", "", 9, 16),
                 ("ParOpen", "", 11, 1),
                 ("String", "\"a string 123 true false\"", 11, 2),
                 ("ParClose", "", 11, 27)]
                    .iter()
                    .map(|&(t, v, l, c)| (t.to_string(), v.to_string(), l, c))
                    .collect();

        for (actual, expected) in token_tuples.iter().zip(expected_token_tuples) {
            assert_eq!(actual, &expected);
        }
    }


    #[test]
    #[should_panic]
    fn error() {
        let mut lexer = Lexer::new("asd123".to_string());
        lexer
            .get_tokens()
            .map_err(|err| println!("{}", err))
            .unwrap();

        let mut lexer = Lexer::new("123asd".to_string());
        lexer
            .get_tokens()
            .map_err(|err| println!("{}", err))
            .unwrap();

        let mut lexer = Lexer::new("\"asd".to_string());
        lexer
            .get_tokens()
            .map_err(|err| println!("{}", err))
            .unwrap();

        let mut lexer = Lexer::new("\"".to_string());
        lexer
            .get_tokens()
            .map_err(|err| println!("{}", err))
            .unwrap();

        let mut lexer = Lexer::new("123\"asd".to_string());
        lexer
            .get_tokens()
            .map_err(|err| println!("{}", err))
            .unwrap();
    }
}
