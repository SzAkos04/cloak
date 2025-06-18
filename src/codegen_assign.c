#include "codegen_assign.h"

#include "ast.h"
#include "codegen_expr.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int codegen_assign(ast_node_t *node, LLVMBuilderRef builder,
                   LLVMModuleRef module, LLVMContextRef context,
                   symbol_table_t *symtab, LLVMValueRef *assign) {
    if (node->type != AST_ASSIGN) {
        error("codegen_assign called with non-assign node");
        return -1;
    }

    // generate pointer to assign to from lhs expression:
    LLVMValueRef ptr = NULL;

    switch (node->assign.lhs->type) {
    case AST_IDENTIFIER: {
        symbol_t *sym =
            symbol_table_lookup(symtab, node->assign.lhs->identifier.name);
        if (!sym) {
            error("undefined variable `%s`", node->assign.lhs->identifier.name);
            return -1;
        }
        if (!sym->is_mutable) {
            error("cannot mutate immutable variable `%s`", sym->name);
            return -1;
        }
        ptr = sym->value; // pointer to variable's storage
        break;
    }
    case AST_INDEX: { // BUG: SIGSEGV here

        // You need to implement codegen for indexed lvalue,
        // something like codegen_index_lvalue() that returns pointer
        if (codegen_expr(node->assign.lhs, builder, module, context, symtab,
                         &ptr) != 0) {
            return -1;
        }
        break;
    }
    default:
        error("unsupported LHS expression in assignment");
        return -1;
    }

    LLVMValueRef val;
    if (codegen_expr(node->assign.value, builder, module, context, symtab,
                     &val) != 0) {
        return -1;
    }

    LLVMTypeRef expected_type = LLVMTypeOf(ptr);
    LLVMTypeRef val_type = LLVMTypeOf(val);

    if (val_type != expected_type) {
        char *expected_str = LLVMPrintTypeToString(expected_type);
        char *actual_str = LLVMPrintTypeToString(val_type);
        error("type mismatch in assignment: expected `%s`, got `%s`",
              expected_str, actual_str);
        LLVMDisposeMessage(expected_str);
        LLVMDisposeMessage(actual_str);
        return -1;
    }

    LLVMBuildStore(builder, val, ptr);

    if (assign) {
        *assign = val;
    }

    return 0;
}
