#include "token.h"

#include "debug.h"

const char *token_type_to_str(token_type_t type) {
    switch (type) {
    case TOKEN_LPAREN:
        return "TOKEN_LPAREN";
    case TOKEN_RPAREN:
        return "TOKEN_RPAREN";
    case TOKEN_LBRACE:
        return "TOKEN_LBRACE";
    case TOKEN_RBRACE:
        return "TOKEN_RBRACE";
    case TOKEN_LBRACKET:
        return "TOKEN_LBRACKET";
    case TOKEN_RBRACKET:
        return "TOKEN_RBRACKET";
    case TOKEN_COLON:
        return "TOKEN_COLON";
    case TOKEN_SEMICOLON:
        return "TOKEN_SEMICOLON";
    case TOKEN_COMMA:
        return "TOKEN_COMMA";
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
    case TOKEN_ELSE:
        return "TOKEN_ELSE";
    case TOKEN_FN:
        return "TOKEN_FN";
    case TOKEN_IF:
        return "TOKEN_IF";
    case TOKEN_LET:
        return "TOKEN_LET";
    case TOKEN_MUT:
        return "TOKEN_MUT";
    case TOKEN_RETURN:
        return "TOKEN_RETURN";
    case TOKEN_WHILE:
        return "TOKEN_WHILE";
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

void debug_token(token_t token) {
    info("{ type: %s; lexeme: \"%s\"; len: %d; line: %d }",
         token_type_to_str(token.type), token.lexeme, token.len, token.line);
}

bool is_unary_op(token_t t) {
    return t.type == TOKEN_MINUS || t.type == TOKEN_BANG;
}

bool is_binary_op(token_t t) {
    return t.type == TOKEN_PLUS || t.type == TOKEN_MINUS ||
           t.type == TOKEN_STAR || t.type == TOKEN_SLASH ||
           t.type == TOKEN_EQUALEQUAL || t.type == TOKEN_BANGEQUAL ||
           t.type == TOKEN_LESS || t.type == TOKEN_LESSEQUAL ||
           t.type == TOKEN_GREATER || t.type == TOKEN_GREATEREQUAL ||
           t.type == TOKEN_AND || t.type == TOKEN_OR;
}
