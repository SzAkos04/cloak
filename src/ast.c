#include "ast.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Helper to print indentation
static void print_indent(int indent) {
    for (int i = 0; i < indent; ++i) {
        printf("  ");
    }
}

char *primary_type_to_str(primary_type_t kind) {
    switch (kind) {
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
        return "unknown primary type";
    }
}

type_t type_void(void) {
    return (type_t){
        .kind = TYPE_PRIMARY,
        .data.primary = TYPE_VOID,
    };
}

void free_type(type_t t) {
    if (t.kind == TYPE_ARRAY) {
        free_type(*t.data.array.type);
        free(t.data.array.type);
    }
}

char *type_to_str(type_t *type) {
    if (!type) {
        return "NULL";
    }
    if (type->kind == TYPE_PRIMARY) {
        return primary_type_to_str(type->data.primary);
    } else {
        char msg[64];
        snprintf(msg, sizeof(msg), "arr<%s, ...>",
                 type_to_str(type->data.array.type));
        return strdup(msg);
    }
}

int str_to_unary_op(const char *str, unary_op_t *op) {
    if (strcmp(str, "-") == 0) {
        *op = UNARY_NEGATE;
        return 0;
    } else if (strcmp(str, "!") == 0) {
        *op = UNARY_NOT;
        return 0;
    }
    return -1;
}

int str_to_binary_op(const char *str, binary_op_t *op) {
    if (strcmp(str, "+") == 0)
        *op = BIN_ADD;
    else if (strcmp(str, "-") == 0)
        *op = BIN_SUB;
    else if (strcmp(str, "*") == 0)
        *op = BIN_MUL;
    else if (strcmp(str, "/") == 0)
        *op = BIN_DIV;
    else if (strcmp(str, "%") == 0)
        *op = BIN_MOD;
    else if (strcmp(str, "==") == 0)
        *op = BIN_EQ;
    else if (strcmp(str, "!=") == 0)
        *op = BIN_NEQ;
    else if (strcmp(str, "<") == 0)
        *op = BIN_LT;
    else if (strcmp(str, ">") == 0)
        *op = BIN_GT;
    else if (strcmp(str, "<=") == 0)
        *op = BIN_LTE;
    else if (strcmp(str, ">=") == 0)
        *op = BIN_GTE;
    else if (strcmp(str, "&&") == 0)
        *op = BIN_AND;
    else if (strcmp(str, "||") == 0)
        *op = BIN_OR;
    else
        return -1;
    return 0;
}

const char *unary_op_to_str(unary_op_t op) {
    switch (op) {
    case UNARY_NEGATE:
        return "-";
    case UNARY_NOT:
        return "!";
    default:
        return "(unknown unary op)";
    }
}

const char *binary_op_to_str(binary_op_t op) {
    switch (op) {
    case BIN_ADD:
        return "+";
    case BIN_SUB:
        return "-";
    case BIN_MUL:
        return "*";
    case BIN_DIV:
        return "/";
    case BIN_MOD:
        return "%";
    case BIN_EQ:
        return "==";
    case BIN_NEQ:
        return "!=";
    case BIN_LT:
        return "<";
    case BIN_GT:
        return ">";
    case BIN_LTE:
        return "<=";
    case BIN_GTE:
        return ">=";
    case BIN_AND:
        return "&&";
    case BIN_OR:
        return "||";
    default:
        return "(unknown binary op)";
    }
}

static void debug_ast_node(ast_node_t *node, int indent) {
    if (!node)
        return;

    print_indent(indent);

    switch (node->type) {
    case AST_PROGRAM:
        printf("AST_PROGRAM with %d declarations:\n", node->program.decl_count);
        for (int i = 0; i < node->program.decl_count; ++i) {
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
            break;
        }
        break;

    case AST_BLOCK:
        printf("AST_BLOCK with %d statements:\n", node->block.stmt_count);
        for (int i = 0; i < node->block.stmt_count; ++i) {
            debug_ast_node(node->block.stmt[i], indent + 1);
        }
        break;

    case AST_ASSIGN:
        printf("AST_ASSIGN:\n");
        print_indent(indent + 1);
        printf("LHS:\n");
        if (node->assign.lhs) {
            debug_ast_node(node->assign.lhs, indent + 2);
        } else {
            print_indent(indent + 2);
            printf("(no lhs)\n");
        }
        print_indent(indent + 1);
        printf("Value:\n");
        if (node->assign.value) {
            debug_ast_node(node->assign.value, indent + 2);
        } else {
            print_indent(indent + 2);
            printf("(no value)\n");
        }
        break;

    case AST_UNARY:
        printf("AST_UNARY: operator=%s\n", unary_op_to_str(node->unary.op));
        debug_ast_node(node->unary.right, indent + 1);
        break;

    case AST_BINARY:
        printf("AST_BINARY: operator=%s\n", binary_op_to_str(node->binary.op));
        debug_ast_node(node->binary.left, indent + 1);
        debug_ast_node(node->binary.right, indent + 1);
        break;

    case AST_CALL:
        printf("AST_CALL:\n");

        print_indent(indent + 1);
        printf("Arguments (%d):\n", node->call.param_count);
        for (int i = 0; i < node->call.param_count; ++i) {
            debug_ast_node(node->call.args[i], indent + 2);
        }
        break;

    case AST_INDEX:
        printf("AST_INDEX:\n");

        print_indent(indent + 1);
        printf("Array:\n");
        debug_ast_node(node->index.array, indent + 2);

        print_indent(indent + 1);
        printf("Index:\n");
        debug_ast_node(node->index.index, indent + 2);
        break;

    case AST_FUNCTION:
        printf("AST_FUNCTION: %s, params=%d, return_type=%s\n", node->func.name,
               node->func.param_count, type_to_str(&node->func.ret_type));

        if (node->func.param_count > 0) {
            print_indent(indent);
            printf("Params:\n");
            for (int i = 0; i < node->func.param_count; ++i) {
                print_indent(indent + 1);
                printf("%s: %s\n", node->func.params[i].name,
                       type_to_str(&node->func.params[i].type));
            }
        }

        print_indent(indent);
        printf("Body:\n");
        debug_ast_node(node->func.body, indent + 1);
        break;

    case AST_IF:
        printf("AST_IF:\n");

        print_indent(indent);
        printf("Condition:\n");
        debug_ast_node(node->if_stmt.condition, indent + 1);

        print_indent(indent);
        printf("Then:\n");
        debug_ast_node(node->if_stmt.then_block, indent + 1);

        print_indent(indent);
        printf("Else:\n");
        debug_ast_node(node->if_stmt.else_block, indent + 1);
        break;

    case AST_LET:
        printf("AST_LET: name=%s, mutable=%s, type=%s\n", node->let.name,
               node->let.is_mutable ? "true" : "false",
               type_to_str(&node->let.type));

        if (node->let.value) {
            print_indent(indent + 1);
            printf("Value:\n");
            debug_ast_node(node->let.value, indent + 2);
        }
        break;

    case AST_RETURN:
        printf("AST_RETURN:\n");
        debug_ast_node(node->return_stmt.value, indent + 1);
        break;

    case AST_WHILE:
        printf("AST_WHILE:\n");

        print_indent(indent);
        printf("Condition:\n");
        debug_ast_node(node->if_stmt.condition, indent + 1);

        print_indent(indent);
        printf("Then:\n");
        debug_ast_node(node->if_stmt.then_block, indent + 1);
        break;

    default:
        printf("AST_UNKNOWN_TYPE: %d\n", node->type);
        break;
    }
}

void debug_ast(ast_t *ast) { debug_ast_node(ast->root, 0); }
