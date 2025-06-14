#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    // one char tokens
    TOKEN_LPAREN,    // `(`
    TOKEN_RPAREN,    // `)`
    TOKEN_LBRACE,    // `{`
    TOKEN_RBRACE,    // `}`
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

    TOKEN_FN,     // `fn`
    TOKEN_LET,    // `let`
    TOKEN_MUT,    // `mut`
    TOKEN_RETURN, // `return`

    TOKEN_IDENTIFIER,

    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_BOOL,

    TOKEN_EOF,
} token_type_t;

static inline const char *token_type_to_str(token_type_t type) {
    switch (type) {
    case TOKEN_LPAREN:
        return "TOKEN_LPAREN";
    case TOKEN_RPAREN:
        return "TOKEN_RPAREN";
    case TOKEN_LBRACE:
        return "TOKEN_LBRACE";
    case TOKEN_RBRACE:
        return "TOKEN_RBRACE";
    case TOKEN_COLON:
        return "TOKEN_COLON";
    case TOKEN_SEMICOLON:
        return "TOKEN_SEMICOLON";
    case TOKEN_EQUAL:
        return "TOKEN_EQUAL";
    case TOKEN_EQUALEQUAL:
        return "TOKEN_EQUALEQUAL";
    case TOKEN_BANG:
        return "TOKEN_BANG";
    case TOKEN_BANGEQUAL:
        return "TOKEN_BANGEQUAL";
    case TOKEN_PLUS:
        return "TOKEN_PLUS";
    case TOKEN_PLUSEQUAL:
        return "TOKEN_PLUSEQUAL";
    case TOKEN_MINUS:
        return "TOKEN_MINUS";
    case TOKEN_MINUSEQUAL:
        return "TOKEN_MINUSEQUAL";
    case TOKEN_STAR:
        return "TOKEN_STAR";
    case TOKEN_STAREQUAL:
        return "TOKEN_STAREQUAL";
    case TOKEN_SLASH:
        return "TOKEN_SLASH";
    case TOKEN_SLASHEQUAL:
        return "TOKEN_SLASHEQUAL";
    case TOKEN_LESS:
        return "TOKEN_LESS";
    case TOKEN_LESSEQUAL:
        return "TOKEN_LESSEQUAL";
    case TOKEN_GREATER:
        return "TOKEN_GREATER";
    case TOKEN_GREATEREQUAL:
        return "TOKEN_GREATEREQUAL";
    case TOKEN_AND:
        return "TOKEN_AND";
    case TOKEN_OR:
        return "TOKEN_OR";
    case TOKEN_FN:
        return "TOKEN_FN";
    case TOKEN_LET:
        return "TOKEN_LET";
    case TOKEN_MUT:
        return "TOKEN_MUT";
    case TOKEN_RETURN:
        return "TOKEN_RETURN";
    case TOKEN_IDENTIFIER:
        return "TOKEN_IDENTIFIER";
    case TOKEN_NUMBER:
        return "TOKEN_NUMBER";
    case TOKEN_STRING:
        return "TOKEN_STRING";
    case TOKEN_BOOL:
        return "TOKEN_BOOL";
    case TOKEN_EOF:
        return "TOKEN_EOF";
    default:
        return NULL;
    }
}

typedef struct {
    token_type_t type;
    char *lexeme;
    int len;
    int line;
} token_t;

void debug_token(token_t token);

static inline bool is_unary_op(token_t t) {
    return t.type == TOKEN_MINUS || t.type == TOKEN_BANG;
}

static inline bool is_binary_op(token_t t) {
    return t.type == TOKEN_PLUS || t.type == TOKEN_MINUS ||
           t.type == TOKEN_STAR || t.type == TOKEN_SLASH ||
           t.type == TOKEN_EQUALEQUAL || t.type == TOKEN_BANGEQUAL ||
           t.type == TOKEN_LESS || t.type == TOKEN_LESSEQUAL ||
           t.type == TOKEN_GREATER || t.type == TOKEN_GREATEREQUAL ||
           t.type == TOKEN_AND || t.type == TOKEN_OR;
}
