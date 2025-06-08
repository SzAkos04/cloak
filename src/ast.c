#include "ast.h"

#include <stdio.h>

static const char *type_to_str(type_t type) {
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
    }
    return NULL;
}

static void debug_ast_node(ast_node_t *node, int indent) {
    if (!node) {
        return;
    }
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
    switch (node->type) {
    case AST_IDENTIFIER:
        printf("AST_IDENTIFIER: %s\n", node->identifier.name);
        break;
    case AST_LITERAL:
        switch (node->literal.kind) {
        case LITERAL_NUMBER:
            printf("AST_LITERAL (number): %g\n", node->literal.number);
            break;
        case LITERAL_STRING:
            printf("AST_LITERAL (string): \"%s\"\n", node->literal.string);
            break;
        case LITERAL_BOOL:
            printf("AST_LITERAL (bool): %s\n",
                   node->literal.boolean ? "true" : "false");
            break;
        default:
            printf("AST_LITERAL (unknown kind)\n");
        }
        break;
    case AST_RETURN:
        printf("AST_RETURN:\n");
        debug_ast_node(node->return_stmt.value, indent + 1);
        break;
    case AST_BLOCK:
        printf("AST_BLOCK with %d statements:\n", node->block.stmt_count);
        for (int i = 0; i < node->block.stmt_count; i++) {
            debug_ast_node(node->block.stmt[i], indent + 1);
        }
        break;
    case AST_FUNCTION:
        printf("AST_FUNCTION: %s, params=%d, return_type=%s\n", node->func.name,
               node->func.param_count, type_to_str(node->func.ret_type));
        if (node->func.param_count > 0) {
            printf("Params:\n");
            for (int i = 0; i < node->func.param_count; i++) {
                debug_ast_node(node->func.params[i], indent + 1);
            }
        }
        printf("Body:\n");
        debug_ast_node(node->func.body, indent + 1);
        break;
    default:
        printf("AST_UNKNOWN_TYPE: %d\n", node->type);
    }
}

void debug_ast(ast_t *ast) { debug_ast_node(ast->root, 0); }
