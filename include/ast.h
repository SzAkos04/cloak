#pragma once

typedef enum {
    AST_FUNCTION,
} ast_node_type_t;

typedef struct ast_node {
    ast_node_type_t type;

    union {
        struct {
            char *name;
            struct ast_node **params;
            int param_count;
            struct ast_node *body;
        } function;
    };
} ast_node_t;

typedef struct {
    ast_node_t *root;
} ast_t;
