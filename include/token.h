#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    // one char tokens
    TOKEN_LPAREN,    // `(`
    TOKEN_RPAREN,    // `)`
    TOKEN_LBRACE,    // `{`
    TOKEN_RBRACE,    // `}`
    TOKEN_LBRACKET,  // `[`
    TOKEN_RBRACKET,  // `]`
    TOKEN_COLON,     // `:`
    TOKEN_SEMICOLON, // `;`
    TOKEN_COMMA,     // `,`

    // one or two char tokens
    TOKEN_EQUAL,        // `=`
    TOKEN_EQUALEQUAL,   // `==`
    TOKEN_BANG,         // `!`
    TOKEN_BANGEQUAL,    // `!=`
    TOKEN_PLUS,         // `+`
    TOKEN_PLUSEQUAL,    // `+=`
    TOKEN_MINUS,        // `-`
    TOKEN_MINUSEQUAL,   // `-=`
    TOKEN_STAR,         // `*`
    TOKEN_STAREQUAL,    // `*=`
    TOKEN_SLASH,        // `/`
    TOKEN_SLASHEQUAL,   // `/=`
    TOKEN_PERCENT,      // `%`
    TOKEN_PERCENTEQUAL, // ``
    TOKEN_LESS,         // `<`
    TOKEN_LESSEQUAL,    // `<=`
    TOKEN_GREATER,      // `>`
    TOKEN_GREATEREQUAL, // >=

    TOKEN_AND, // `&&`
    TOKEN_OR,  // `||`

    TOKEN_ELSE,   // `else`
    TOKEN_FN,     // `fn`
    TOKEN_IF,     // `if`
    TOKEN_LET,    // `let`
    TOKEN_MUT,    // `mut`
    TOKEN_RETURN, // `return`
    TOKEN_WHILE,  // `while`

    TOKEN_IDENTIFIER,

    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_BOOL,

    TOKEN_EOF,
} token_type_t;

const char *token_type_to_str(token_type_t type);

typedef struct {
    token_type_t type;
    char *lexeme;
    int len;
    int line;
} token_t;

void debug_token(token_t token);

bool is_unary_op(token_t t);
bool is_binary_op(token_t t);
