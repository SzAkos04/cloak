#include "lexer.h"

#include "debug.h"
#include "token.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static lexer_t lexer;

void lexer_init(char *src) {
    lexer.start = src;
    lexer.current = src;
    lexer.line = 1;
}

static char peek(void) { return *lexer.current; }

static char advance(void) { return *lexer.current++; }

static bool is_at_end(void) { return *lexer.current == '\0'; }

static int make_token(token_type_t type, token_t *token) {
    int len = lexer.current - lexer.start;
    char *lexeme = (char *)malloc(len + 1);
    if (!lexeme) {
        perr("Failed to allocate memory for lexeme");
        return -1;
    }

    memcpy(lexeme, lexer.start, len);
    lexeme[len] = '\0';
    *token = (token_t){
        .type = type,
        .lexeme = lexeme,
        .len = len,
        .line = lexer.line,
    };

    return 0;
}

static void skip_whitespace(void) {
    while (!is_at_end()) {
        char c = peek();
        if (c == ' ' || c == '\r') {
            advance();
        } else if (c == '\n') {
            lexer.line++;
            advance();
        } else {
            break;
        }
    }
}

static int identifier(token_t *token) {
    while (isalnum(peek()) || peek() == '_') {
        advance();
    }

    int len = (int)(lexer.current - lexer.start);
    if (len == 2 && strncmp(lexer.start, "fn", 2) == 0) {
        return make_token(TOKEN_FN, token);
    }

    return make_token(TOKEN_IDENTIFIER, token);
}

static int number(token_t *token) {
    while (isdigit(peek())) {
        advance();
    }
    return make_token(TOKEN_NUMBER, token);
}

static int next_token(token_t *token) {
    skip_whitespace();

    lexer.start = lexer.current;

    if (is_at_end()) {
        return make_token(TOKEN_EOF, token);
    }

    char c = advance();

    switch (c) {
    case '(':
        return make_token(TOKEN_LPAREN, token);
    case ')':
        return make_token(TOKEN_RPAREN, token);
    case '{':
        return make_token(TOKEN_LBRACE, token);
    case '}':
        return make_token(TOKEN_RBRACE, token);
    case ':':
        return make_token(TOKEN_COLON, token);
    case ';':
        return make_token(TOKEN_SEMICOLON, token);
    case '=':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_EQUALEQUAL, token);
        } else {
            return make_token(TOKEN_EQUAL, token);
        }
    case '!':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_BANGEQUAL, token);
        } else {
            return make_token(TOKEN_BANG, token);
        }
    default:
        if (isdigit(c)) {
            return number(token);
        }
        if (isalpha(c) || c == '_') {
            return identifier(token);
        }
        error("unknown token at line %d: `%s`", token->line, token->lexeme);
        return -1;
        // return make_token(TOKEN_UNKNOWN, token);
    }
}

#define INITIAL_CAPACITY 64

int lex(token_t **tokens) {
    int capacity = INITIAL_CAPACITY;
    int count = 0;
    *tokens = (token_t *)malloc(capacity * sizeof(token_t));
    if (!*tokens) {
        perr("Failed to allocate memory to `tokens`");
        return -1;
    }

    token_t token;
    do {
        if (next_token(&token) != 0) {
            free_tokens(*tokens, count);
            return -1;
        }

        if (count >= capacity) {
            capacity *= 2;
            token_t *new_tokens =
                (token_t *)realloc(*tokens, capacity * sizeof(token_t));
            if (!new_tokens) {
                perr("Failed to allocate memory to `tokens`");
                free(*tokens);
                return -1;
            }
            *tokens = new_tokens;
        }

        (*tokens)[count++] = token;

    } while (token.type != TOKEN_EOF);

    return (int)count;
}

void free_tokens(token_t *tokens, int n) {
    for (int i = 0; i < n; ++i) {
        free(tokens[i].lexeme);
    }
    free(tokens);
}
