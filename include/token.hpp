#pragma once

#include <string>

enum class TokenType {
    // one char tokens
    LPAREN,    // `(`
    RPAREN,    // `)`
    LBRACE,    // `{`
    RBRACE,    // `}`
    LBRACKET,  // `[`
    RBRACKET,  // `]`
    COLON,     // `:`
    SEMICOLON, // `;`
    COMMA,     // `,`

    // one or two char tokens
    EQUAL,        // `=`
    EQUALEQUAL,   // `==`
    BANG,         // `!`
    BANGEQUAL,    // `!=`
    PLUS,         // `+`
    PLUSEQUAL,    // `+=`
    MINUS,        // `-`
    MINUSEQUAL,   // `-=`
    STAR,         // `*`
    STAREQUAL,    // `*=`
    SLASH,        // `/`
    SLASHEQUAL,   // `/=`
    PERCENT,      // `%`
    PERCENTEQUAL, // `%=`
    LESS,         // `<`
    LESSEQUAL,    // `<=`
    GREATER,      // `>`
    GREATEREQUAL, // >=

    AND, // `&&`
    OR,  // `||`

    IDENTIFIER,

    FN,     // `fn`
    RETURN, // `return`

    NUMBER,
    STRING,
    BOOL,

    EOF_,
};

class Token {
  public:
    Token(TokenType type, const std::string &lexeme, int len, int line);
    friend std::ostream &operator<<(std::ostream &os, const Token &tok);

    TokenType getType() const { return this->type; }
    const std::string &getLexeme() const { return this->lexeme; }
    int getLine() const { return this->line; }
    int getLength() const { return this->len; }

  private:
    TokenType type;
    std::string lexeme;
    int len;
    int line;
};
