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
} primary_type_t;
char *primary_type_to_str(primary_type_t kind);

typedef enum {
    TYPE_PRIMARY,
    TYPE_ARRAY,
} typekind_t;

typedef struct type_t {
    typekind_t kind;

    union {
        primary_type_t primary;

        struct {
            struct type_t *type;
            int length;
        } array;
    } data;
} type_t;
type_t type_void(void);
void free_type(type_t t);

// MUST BE FREED
char *type_to_str(type_t *type);

typedef struct {
    enum { LITERAL_NUMBER, LITERAL_STRING, LITERAL_BOOL } kind;
    union {
        struct {
            double number;
            primary_type_t num_type;
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

    AST_CALL,  // for function calls
    AST_INDEX, // for array access

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
    struct ast_node **args; // expr *
    int param_count;
} ast_call_t;

typedef struct {
    struct ast_node *array; // identifier
    struct ast_node *index; // expr
} ast_index_t;

typedef struct {
    char *name;
    param_t *params;
    int param_count;
    type_t ret_type;
    struct ast_node *body; // block
} ast_function_t;

typedef struct {
    struct ast_node *condition;  // expr
    struct ast_node *then_block; // block
    struct ast_node *else_block; // block
} ast_if_t;

typedef struct {
    char *name;
    bool is_mutable;
    type_t type;
    struct ast_node *value; // expr
} ast_let_t;

typedef struct {
    struct ast_node *value; // expr
} ast_return_t;

typedef struct {
    struct ast_node *condition; // expr
    struct ast_node *body;      // block
} ast_while_t;

typedef struct {
    char *name;
} ast_identifier_t;

typedef struct {
    struct ast_node **stmt;
    int stmt_count;
} ast_block_t;

typedef struct {
    struct ast_node *lhs;
    struct ast_node *value; // expr
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

typedef struct ast_node {
    ast_node_type_t type;

    union {
        ast_program_t program;

        ast_identifier_t identifier;
        literal_t literal;
        ast_block_t block;
        ast_assign_t assign;
        ast_unary_t unary;
        ast_binary_t binary;

        ast_call_t call;
        ast_index_t index;

        ast_function_t func;
        ast_if_t if_stmt;
        ast_let_t let;
        ast_return_t return_stmt;
        ast_while_t while_stmt;
    };
} ast_node_t;

typedef struct {
    ast_node_t *root;
} ast_t;

void debug_ast(ast_t *ast);
