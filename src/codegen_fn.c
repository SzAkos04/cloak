#include "codegen_fn.h"

#include "ast.h"
#include "codegen_block.h"
#include "codegen_utils.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <stdlib.h>

int declare_fn(ast_node_t *node, LLVMModuleRef module, LLVMContextRef context,
               symbol_table_t *symtab) {
    if (node->type != AST_FUNCTION)
        return -1;

    int param_count = node->func.param_count;
    LLVMTypeRef *param_types =
        (LLVMTypeRef *)malloc(sizeof(LLVMTypeRef) * param_count);
    if (!param_types) {
        perr("codegen_fn: failed to allocate memory for `param_types`");
        return -1;
    }

    for (int i = 0; i < param_count; ++i) {
        param_types[i] = get_llvm_type(node->func.params[i].type, context);
        if (!param_types[i]) {
            free(param_types);
            return -1;
        }
    }

    LLVMTypeRef ret_type = get_llvm_type(node->func.ret_type, context);
    LLVMTypeRef func_type =
        LLVMFunctionType(ret_type, param_types, param_count, false);
    LLVMValueRef function = LLVMAddFunction(module, node->func.name, func_type);

    if (symbol_table_add(symtab, node->func.name, func_type, function, false) !=
        0) {
        free(param_types);
        return -1;
    }
    free(param_types);
    return 0;
}

int codegen_fn(ast_node_t *node, LLVMModuleRef module, LLVMContextRef context,
               symbol_table_t *symtab, LLVMValueRef *func) {
    if (node->type != AST_FUNCTION) {
        error("codegen_function called with non-function node");
        return -1;
    }

    // Lookup previously declared function
    symbol_t *sym_entry = symbol_table_lookup(symtab, node->func.name);
    if (!sym_entry) {
        error("codegen_fn: function '%s' not found in symbol table",
              node->func.name);
        return -1;
    }
    LLVMValueRef function = sym_entry->value;
    if (!function) {
        error("codegen_fn: function not declared before body generation");
        return -1;
    }

    int param_count = node->func.param_count;
    symbol_table_t local_symtab = {0};
    local_symtab.head = NULL;

    LLVMBasicBlockRef entry =
        LLVMAppendBasicBlockInContext(context, function, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilderInContext(context);
    LLVMPositionBuilderAtEnd(builder, entry);

    for (int i = 0; i < param_count; ++i) {
        LLVMValueRef param_val = LLVMGetParam(function, i);
        LLVMTypeRef param_type = LLVMTypeOf(param_val);

        LLVMValueRef alloca_inst =
            LLVMBuildAlloca(builder, param_type, node->func.params[i].name);
        LLVMBuildStore(builder, param_val, alloca_inst);

        symbol_table_add(&local_symtab, node->func.params[i].name, param_type,
                         alloca_inst, true);
    }

    // rename for readability
    for (int i = 0; i < param_count; ++i) {
        LLVMSetValueName(LLVMGetParam(function, i), node->func.params[i].name);
    }

    LLVMValueRef ret_val;
    if (codegen_block(node->func.body, builder, module, context, &local_symtab,
                      &ret_val) != 0) {
        symbol_table_free(&local_symtab);
        LLVMDisposeBuilder(builder);
        return -1;
    }

    LLVMTypeRef ret_type = get_llvm_type(node->func.ret_type, context);

    if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder))) {
        if (node->func.ret_type.kind == TYPE_PRIMARY &&
            node->func.ret_type.data.primary == TYPE_VOID) {
            LLVMBuildRetVoid(builder);
        } else if (ret_val) {
            LLVMBuildRet(builder, ret_val);
        } else {
            LLVMBuildRet(builder, LLVMConstNull(ret_type));
        }
    }

    symbol_table_free(&local_symtab);
    LLVMDisposeBuilder(builder);

    *func = function;
    return 0;
}
