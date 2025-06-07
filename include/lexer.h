#pragma once

#include "token.h"

typedef struct {
    char *start;
    char *current;
    int line;
} lexer_t;
void lexer_init(char *source);

int lex(token_t **tokens);
void free_tokens(token_t *tokens, int n);
