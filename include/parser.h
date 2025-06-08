#pragma once

#include "ast.h"
#include "token.h"

typedef struct {
    token_t *tokens;
    int count;
    int current;
} parser_t;
void parser_init(token_t *tokens, int count);

int parse_ast(ast_t **ast);

void free_ast(ast_t *ast);
