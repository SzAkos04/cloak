#pragma once

#include <stdbool.h>

typedef enum {
    TYPE_BOOL,
    TYPE_F32,
    TYPE_F64,
    TYPE_I8,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64,
    TYPE_STRING,
    TYPE_VOID,
} type_t;

#define TYPE_TO_STR(type)                                                      \
    ((type) == TYPE_BOOL     ? "bool"                                          \
     : (type) == TYPE_F32    ? "f32"                                           \
     : (type) == TYPE_F64    ? "f64"                                           \
     : (type) == TYPE_I8     ? "i8"                                            \
     : (type) == TYPE_I16    ? "i16"                                           \
     : (type) == TYPE_I32    ? "i32"                                           \
     : (type) == TYPE_I64    ? "i64"                                           \
     : (type) == TYPE_STRING ? "string"                                        \
     : (type) == TYPE_VOID   ? "void"                                          \
                             : "unknown")

typedef enum {
    LITERAL_NUMBER,
    LITERAL_STRING,
    LITERAL_BOOL,
} literal_type_t;

typedef struct {
    literal_type_t kind;
    union {
        struct {
            double number;
            type_t num_type;
        };
        char *string;
        bool boolean;
    };
} literal_t;

typedef enum {
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_BLOCK,

    AST_FUNCTION,
    AST_RETURN,
} ast_node_type_t;

struct ast_node;

typedef struct {
    struct ast_node **decls;
    int decl_count;
} ast_program_t;

typedef struct {
    char *name;
    struct ast_node **params;
    int param_count;
    type_t ret_type;
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
        ast_program_t program;
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
