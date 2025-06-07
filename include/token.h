#pragma once

typedef enum {
    // one char tokens
    TOKEN_LPAREN,    // `(`
    TOKEN_RPAREN,    // `)`
    TOKEN_LBRACE,    // `{`
    TOKEN_RBRACE,    // `}`
    TOKEN_COLON,     // `:`
    TOKEN_SEMICOLON, // `;`

    // one or two char tokens
    TOKEN_EQUAL,      // `=`
    TOKEN_EQUALEQUAL, // `==`
    TOKEN_BANG,       // `!`
    TOKEN_BANGEQUAL,  // `!=`
    TOKEN_FN,         // `fn`
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_EOF,
    TOKEN_UNKNOWN,
} token_type_t;

typedef struct {
    token_type_t type;
    char *lexeme;
    int len;
    int line;
} token_t;

void debug_token(token_t token);
