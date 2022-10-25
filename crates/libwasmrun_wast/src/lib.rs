#[derive(Debug, PartialEq, Eq)]
pub enum Token<'input> {
    LParen,
    RParen,
    Keyword(&'input str),
    Id(&'input str),
    String(Vec<u8>),
}

#[derive(Debug, Default)]
pub struct LexerState {
    block_comment_nesting: u32,
    string_buf: Vec<u8>,
}

lexgen::lexer! {
    pub Lexer(LexerState) -> Token<'input>;

    // https://webassembly.github.io/spec/core/text/values.html#text-idchar
    let idchar = ['0'-'9' 'A'-'Z' 'a'-'z' '!' '#' '$' '%' '&' '\'' '*' '+' '-' '.' '/' ':' '<' '='
        '>' '?' '@' '\\' '^' '_' '`' '|' '~'];

    let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F'];

    rule Init {
        $$whitespace,

        ";;" _* '\n',

        "(;" => |lexer| {
            lexer.state().block_comment_nesting = 1;
            lexer.switch(LexerRule::BlockComment)
        },

        '(' = Token::LParen,

        ')' = Token::RParen,

        '$' $idchar+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Id(match_))
        },

        ['a'-'z'] $idchar* => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Keyword(match_))
        },

        '"' => |lexer| {
            lexer.state().string_buf.clear();
            lexer.switch(LexerRule::String)
        },
    }

    rule BlockComment {
        ";)" => |lexer| {
            if lexer.state().block_comment_nesting == 1 {
                lexer.switch(LexerRule::Init)
            } else {
                lexer.state().block_comment_nesting -= 1;
                lexer.continue_()
            }
        },

        "(;" => |lexer| {
            lexer.state().block_comment_nesting += 1;
            lexer.continue_()
        },

        _,
    }

    rule String {
        "\\t" => |lexer| {
            lexer.state().string_buf.push(b'\t');
            lexer.continue_()
        },

        "\\n" => |lexer| {
            lexer.state().string_buf.push(b'\n');
            lexer.continue_()
        },

        "\\r" => |lexer| {
            lexer.state().string_buf.push(b'\r');
            lexer.continue_()
        },

        "\\\"" => |lexer| {
            lexer.state().string_buf.push(b'"');
            lexer.continue_()
        },

        "\\'" => |lexer| {
            lexer.state().string_buf.push(b'\'');
            lexer.continue_()
        },

        "\\\\" => |lexer| {
            lexer.state().string_buf.push(b'\\');
            lexer.continue_()
        },

        "\\u{" $hexdigit+ "}" => |lexer| {
            let match_ = lexer.match_();
            let match_ = &match_[3..match_.len() - 1];
            let bytes = u32::from_str_radix(match_, 16).unwrap().to_le_bytes();
            lexer.state().string_buf.extend_from_slice(&bytes);
            lexer.continue_()
        },

        '"' => |lexer| {
            let str = std::mem::take(&mut lexer.state().string_buf);
            lexer.switch_and_return(LexerRule::Init, Token::String(str))
        },

        _ => |lexer| {
            let match_ = lexer.match_();
            let char = match_.chars().last().unwrap();
            use std::io::Write;
            write!(&mut lexer.state().string_buf, "{}", char).unwrap();
            lexer.continue_()
        },
    }
}

#[test]
fn comment_lexing() {
    let mut lexer = Lexer::new("(module\n   ;; asdf asdf  \n   b ");
    assert_eq!(lexer.next().unwrap().unwrap().1, Token::LParen);
    assert_eq!(lexer.next().unwrap().unwrap().1, Token::Keyword("module"));
    assert_eq!(lexer.next().unwrap().unwrap().1, Token::Keyword("b"));
    assert_eq!(lexer.next(), None);
}
