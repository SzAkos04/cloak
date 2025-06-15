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

static inline const char *type_to_str(type_t type) {
    switch (type) {
    case TYPE_BOOL:
        return "bool";
    case TYPE_F32:
        return "f32";
    case TYPE_F64:
        return "f64";
    case TYPE_I8:
        return "i8";
    case TYPE_I16:
        return "i16";
    case TYPE_I32:
        return "i32";
    case TYPE_I64:
        return "i64";
    case TYPE_STRING:
        return "string";
    case TYPE_VOID:
        return "void";
    default:
        return "unknown";
    }
}

typedef struct {
    enum { LITERAL_NUMBER, LITERAL_STRING, LITERAL_BOOL } kind;
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
    UNARY_NEGATE,
    UNARY_NOT,
} unary_op_t;
int str_to_unary_op(const char *str, unary_op_t *op);
const char *unary_op_to_str(unary_op_t op);

typedef enum {
    BIN_ADD,
    BIN_SUB,
    BIN_MUL,
    BIN_DIV,
    BIN_MOD,
    BIN_EQ,
    BIN_NEQ,
    BIN_LT,
    BIN_LTE,
    BIN_GT,
    BIN_GTE,
    BIN_AND,
    BIN_OR
} binary_op_t;
int str_to_binary_op(const char *str, binary_op_t *op);
const char *binary_op_to_str(binary_op_t op);

typedef enum {
    AST_PROGRAM,

    AST_IDENTIFIER,
    AST_LITERAL,
    AST_BLOCK,
    AST_ASSIGN,
    AST_UNARY,
    AST_BINARY,

    AST_CALL,

    AST_FUNCTION,
    AST_IF,
    AST_LET,
    AST_RETURN,
    AST_WHILE,
} ast_node_type_t;

struct ast_node;

typedef struct {
    struct ast_node **decls;
    int decl_count;
} ast_program_t;

typedef struct {
    char *name;
    type_t type;
} param_t;

typedef struct {
    char *name;
    param_t *params;
    int param_count;
    type_t ret_type;
    struct ast_node *body;
} ast_function_t;

typedef struct {
    struct ast_node *condition;
    struct ast_node *then_block;
    struct ast_node *else_block;
} ast_if_t;

typedef struct {
    char *name;
    bool is_mutable;
    type_t type;
    struct ast_node *value;
} ast_let_t;

typedef struct {
    struct ast_node *value;
} ast_return_t;

typedef struct {
    struct ast_node *condition;
    struct ast_node *body;
} ast_while_t;

typedef struct {
    char *name;
} ast_identifier_t;

typedef struct {
    struct ast_node **stmt;
    int stmt_count;
} ast_block_t;

typedef struct {
    char *name;
    struct ast_node *value;
} ast_assign_t;

typedef struct {
    unary_op_t op;
    struct ast_node *right;
} ast_unary_t;

typedef struct {
    struct ast_node *left;
    binary_op_t op;
    struct ast_node *right;
} ast_binary_t;

typedef struct {
    char *name;
    struct ast_node **args;
    int param_count;
} ast_call_t;

typedef struct ast_node {
    ast_node_type_t type;

    union {
        ast_program_t program;

        literal_t literal;
        ast_function_t func;
        ast_if_t if_stmt;
        ast_let_t let;
        ast_return_t return_stmt;
        ast_while_t while_stmt;
        ast_identifier_t identifier;
        ast_block_t block;
        ast_assign_t assign;
        ast_unary_t unary;
        ast_binary_t binary;
        ast_call_t call;
    };
} ast_node_t;

typedef struct {
    ast_node_t *root;
} ast_t;

void debug_ast(ast_t *ast);
