#include "ast.h"

#include <stdio.h>

static void debug_ast_node(ast_node_t *node, int indent) {
    if (!node) {
        return;
    }
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
    switch (node->type) {
    case AST_PROGRAM:
        printf("AST_PROGRAM with %d declarations:\n", node->program.decl_count);
        for (int i = 0; i < node->program.decl_count; i++) {
            debug_ast_node(node->program.decls[i], indent + 1);
        }
        break;
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
    case AST_BLOCK:
        printf("AST_BLOCK with %d statements:\n", node->block.stmt_count);
        for (int i = 0; i < node->block.stmt_count; i++) {
            debug_ast_node(node->block.stmt[i], indent + 1);
        }
        break;
    case AST_ASSIGN:
        printf("AST_ASSIGN: %s =\n", node->assign.name);
        if (node->assign.value) {
            debug_ast_node(node->assign.value, indent + 1);
        } else {
            for (int i = 0; i < indent + 1; i++) {
                printf("  ");
            }
            printf("(no value)\n");
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
    case AST_LET:
        printf("AST_LET: name=%s, mutable=%s, type=%s\n", node->let.name,
               node->let.is_mutable ? "true" : "false",
               type_to_str(node->let.type));

        if (node->let.value) {
            for (int i = 0; i < indent + 1; i++) {
                printf("  ");
            }
            printf("Value:\n");
            debug_ast_node(node->let.value, indent + 2);
        }
        break;
    case AST_RETURN:
        printf("AST_RETURN:\n");
        debug_ast_node(node->return_stmt.value, indent + 1);
        break;
    default:
        printf("AST_UNKNOWN_TYPE: %d\n", node->type);
    }
}

void debug_ast(ast_t *ast) { debug_ast_node(ast->root, 0); }
