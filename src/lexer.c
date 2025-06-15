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
        perr("lexer: failed to allocate memory for lexeme");
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
        } else if (c == '/' && lexer.current[1] == '/') {
            while (!is_at_end() && peek() != '\n') {
                advance();
            }
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
    if (len == strlen("else") &&
        strncmp(lexer.start, "else", strlen("else")) == 0) {
        return make_token(TOKEN_ELSE, token);
    } else if (len == strlen("fn") &&
               strncmp(lexer.start, "fn", strlen("fn")) == 0) {
        return make_token(TOKEN_FN, token);
    } else if (len == strlen("if") &&
               strncmp(lexer.start, "if", strlen("if")) == 0) {
        return make_token(TOKEN_IF, token);
    } else if (len == strlen("let") &&
               strncmp(lexer.start, "let", strlen("let")) == 0) {
        return make_token(TOKEN_LET, token);
    } else if (len == strlen("mut") &&
               strncmp(lexer.start, "mut", strlen("mut")) == 0) {
        return make_token(TOKEN_MUT, token);
    } else if (len == strlen("return") &&
               strncmp(lexer.start, "return", strlen("return")) == 0) {
        return make_token(TOKEN_RETURN, token);
    } else if (len == strlen("true") &&
               strncmp(lexer.start, "true", strlen("true")) == 0) {
        return make_token(TOKEN_BOOL, token);
    } else if (len == strlen("false") &&
               strncmp(lexer.start, "false", strlen("false")) == 0) {
        return make_token(TOKEN_BOOL, token);
    }

    return make_token(TOKEN_IDENTIFIER, token);
}

static int number(token_t *token) {
    while (isdigit(peek())) {
        advance();
    }
    return make_token(TOKEN_NUMBER, token);
}

// TODO: support escape codes
static int string(token_t *token) {
    while (!is_at_end() && peek() != '"') {
        if (peek() == '\n') {
            lexer.line++;
        }
        advance();
    }

    if (is_at_end()) {
        error("unterminated string literal at line %d", lexer.line);
        return -1;
    }

    advance();

    int len = (int)(lexer.current - lexer.start);
    char *lexeme = (char *)malloc(len - 1);
    if (!lexeme) {
        perr("lexer: failed to allocate memory for string literal");
        return -1;
    }

    memcpy(lexeme, lexer.start + 1, len - 2);
    lexeme[len - 2] = '\0';

    *token = (token_t){
        .type = TOKEN_STRING,
        .lexeme = lexeme,
        .len = len - 2,
        .line = lexer.line,
    };

    return 0;
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
    case ',':
        return make_token(TOKEN_COMMA, token);
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
    case '+':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_PLUSEQUAL, token);
        } else {
            return make_token(TOKEN_PLUS, token);
        }
    case '-':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_MINUSEQUAL, token);
        } else {
            return make_token(TOKEN_MINUS, token);
        }
    case '*':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_STAREQUAL, token);
        } else {
            return make_token(TOKEN_STAR, token);
        }
    case '/':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_SLASHEQUAL, token);
        } else {
            return make_token(TOKEN_SLASH, token);
        }
    case '%':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_PERCENTEQUAL, token);
        } else {
            return make_token(TOKEN_PERCENT, token);
        }
    case '<':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_LESSEQUAL, token);
        } else {
            return make_token(TOKEN_LESS, token);
        }
    case '>':
        if (peek() == '=') {
            advance();
            return make_token(TOKEN_GREATEREQUAL, token);
        } else {
            return make_token(TOKEN_GREATER, token);
        }
    case '&':
        if (peek() == '&') {
            advance();
            return make_token(TOKEN_AND, token);
        } else {
            error("unexpected character '%c' at line %d: unrecognized token", c,
                  lexer.line);
            return -1;
        }
    case '|':
        if (peek() == '|') {
            advance();
            return make_token(TOKEN_OR, token);
        } else {
            error("unexpected character '%c' at line %d: unrecognized token", c,
                  lexer.line);
            return -1;
        }
    case '"':
        return string(token);
    default:
        if (isdigit(c)) {
            return number(token);
        }
        if (isalpha(c) || c == '_') {
            return identifier(token);
        }
        error("unexpected character '%c' at line %d: unrecognized token", c,
              lexer.line);
        return -1;
    }
}

#define INITIAL_CAPACITY 64

int lex(token_t **tokens) {
    int capacity = INITIAL_CAPACITY;
    int count = 0;
    *tokens = (token_t *)malloc(capacity * sizeof(token_t));
    if (!*tokens) {
        perr("lexer: failed to allocate memory for token array");
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
                perr("lexer: failed to allocate memory for token array");
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
