#include "codegen_const_expr.h"

#include "ast.h"
#include "debug.h"

int evaluate_constant_expr(ast_node_t *expr, int *out_value) {
    if (!expr) {
        error("`expr` is NULL");
        return -1;
    }

    switch (expr->type) {
    case AST_LITERAL:
        if (expr->literal.kind == LITERAL_NUMBER) {
            *out_value = (int)expr->literal.number;
            return 0;
        }
        break;
    case AST_BINARY: {
        int left, right;
        if (evaluate_constant_expr(expr->binary.left, &left) != 0 ||
            evaluate_constant_expr(expr->binary.right, &right) != 0) {
            return -1;
        }
        switch (expr->binary.op) {
        case BIN_ADD:
            *out_value = left + right;
            return 0;
        case BIN_SUB:
            *out_value = left - right;
            return 0;
        case BIN_MUL:
            *out_value = left * right;
            return 0;
        case BIN_DIV:
            if (right != 0) {
                *out_value = left / right;
                return 0;
            }
            break;
        case BIN_MOD:
            *out_value = left % right;
            return 0;
        default:
            break;
        }
        break;
    }
    default:
        error("unknown statement");
        return -1;
    }
    __builtin_unreachable();
}
