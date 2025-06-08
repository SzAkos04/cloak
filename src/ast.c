#include "ast.h"

#include <stdio.h>

static void debug_ast_node(ast_node_t *node, int indent) {
    if (!node)
        return;
    for (int i = 0; i < indent; i++)
        printf("  ");
    switch (node->type) {
    case AST_IDENTIFIER:
        printf("AST_IDENTIFIER: %s\n", node->identifier.name);
        break;
    case AST_LITERAL:
        switch (node->literal.kind) {
        case LITERAL_NUMBER:
            printf("AST_LITERAL (number): %f\n", node->literal.number);
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
        printf("AST_FUNCTION: %s, params=%d\n", node->func.name,
               node->func.param_count);
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
