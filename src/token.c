#include "debug.h"
#include "token.h"

/*

    // one char tokens
    TOKEN_LPAREN, // `(`
    TOKEN_RPAREN, // `)`
    TOKEN_LBRACE, // `{`
    TOKEN_RBRACE, // `}`

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
*/

static const char *token_type_to_str(token_type_t type) {
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
    case TOKEN_FN:
        return "TOKEN_FN";
    case TOKEN_IDENTIFIER:
        return "TOKEN_IDENTIFIER";
    case TOKEN_NUMBER:
        return "TOKEN_NUMBER";
    case TOKEN_EOF:
        return "TOKEN_EOF";
    case TOKEN_UNKNOWN:
        return "TOKEN_UNKNOWN";
    default:
        return NULL;
    }
}

void debug_token(token_t token) {
    info("{ type: %s; lexeme: \"%s\"; len: %d; line: %d }",
         token_type_to_str(token.type), token.lexeme, token.len, token.line);
}
