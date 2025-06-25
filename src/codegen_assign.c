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

    symbol_t *sym;
    LLVMTypeRef expected_type = NULL;
    switch (node->assign.lhs->type) {
    case AST_IDENTIFIER: {
        sym = symbol_table_lookup(symtab, node->assign.lhs->identifier.name);
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
    case AST_INDEX: {
        ast_node_t *array_expr = node->assign.lhs->index.array;
        if (array_expr->type != AST_IDENTIFIER) {
            error("complex indexing expressions not yet supported");
            return -1;
        }

        sym = symbol_table_lookup(symtab, array_expr->identifier.name);
        if (!sym) {
            error("undefined variable `%s`", array_expr->identifier.name);
            return -1;
        }
        if (!sym->is_mutable) {
            error("cannot mutate immutable variable `%s`", sym->name);
            return -1;
        }
        LLVMValueRef arr_ptr = sym->value;

        LLVMValueRef index_val;
        if (codegen_expr(node->assign.lhs->index.index, builder, module,
                         context, symtab, &index_val) != 0) {
            return -1;
        }

        LLVMTypeRef sym_type = sym->type;                        // [N x T]
        LLVMTypeRef indexed_type = LLVMGetElementType(sym_type); // T

        LLVMValueRef indices[2] = {
            LLVMConstInt(LLVMInt32TypeInContext(context), 0, false), index_val};
        ptr = LLVMBuildInBoundsGEP2(builder, sym_type, arr_ptr, indices, 2,
                                    "elemptr");

        expected_type = indexed_type;
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

    if (!expected_type) {
        expected_type = sym->type;
    }
    LLVMTypeRef val_type = LLVMTypeOf(val);

    if (val_type != expected_type) {
        LLVMDumpModule(module);
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
