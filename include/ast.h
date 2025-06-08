#pragma once

#include "literal.h"

typedef enum {
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_BLOCK,

    AST_FUNCTION,
    AST_RETURN,
} ast_node_type_t;

struct ast_node;

typedef struct {
    char *name;
    struct ast_node **params;
    int param_count;
    struct ast_node *body;
} ast_function_t;

typedef struct {
    struct ast_node *value;
} ast_return_t;

typedef struct {
    char *name;
} ast_identifier_t;

typedef struct {
    struct ast_node **stmt;
    int stmt_count;
} ast_block_t;

typedef struct ast_node {
    ast_node_type_t type;

    union {
        ast_function_t func;
        ast_return_t return_stmt;
        ast_identifier_t identifier;
        ast_block_t block;
        literal_t literal;
    };
} ast_node_t;

typedef struct {
    ast_node_t *root;
} ast_t;

void debug_ast(ast_t *ast);
