#include "codegen_fn.h"

#include "codegen_block.h"
#include "codegen_utils.h"
#include "debug.h"

#include <llvm-c/Core.h>
#include <stdlib.h>

int codegen_fn(ast_node_t *node, LLVMModuleRef module, LLVMContextRef context,
               symbol_table_t *symtab, LLVMValueRef *func) {
    (void)symtab;
    if (node->type != AST_FUNCTION) {
        error("codegen_function called with non-function node");
        return -1;
    }

    int param_count = node->func.param_count;
    LLVMTypeRef *param_types = NULL;
    if (param_count > 0) {
        param_types = (LLVMTypeRef *)malloc(sizeof(LLVMTypeRef) * param_count);
        if (!param_types) {
            perr("codegen: failed to allocate memory for `param_types` "
                 "LLVMTypeRef");
            return -1;
        }
        for (int i = 0; i < param_count; i++) {
            param_types[i] = get_llvm_type(node->func.params[i].type, context);
            if (!param_types[i]) {
                free(param_types);
                return -1;
            }
        }
    }

    LLVMTypeRef ret_type = get_llvm_type(node->func.ret_type, context);
    if (!ret_type) {
        free(param_types);
        return -1;
    }

    LLVMTypeRef func_type =
        LLVMFunctionType(ret_type, param_types, param_count, false);
    free(param_types);

    LLVMValueRef function = LLVMAddFunction(module, node->func.name, func_type);

    LLVMBasicBlockRef entry =
        LLVMAppendBasicBlockInContext(context, function, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilderInContext(context);
    LLVMPositionBuilderAtEnd(builder, entry);

    symbol_table_t local_symtab = {0};
    local_symtab.head = NULL;

    for (int i = 0; i < param_count; i++) {
        LLVMValueRef param_val = LLVMGetParam(function, i);
        LLVMTypeRef param_type = LLVMTypeOf(param_val);

        // create alloca in entry block to store param
        LLVMValueRef alloca_inst =
            LLVMBuildAlloca(builder, param_type, node->func.params[i].name);
        LLVMBuildStore(builder, param_val, alloca_inst);

        // add param variable to symbol table as mutable by default
        symbol_table_add(&local_symtab, node->func.params[i].name, param_type,
                         alloca_inst, true);
    }

    LLVMValueRef ret_val;
    if (codegen_block(node->func.body, builder, module, context, &local_symtab,
                      &ret_val) != 0) {
        symbol_table_free(&local_symtab);
        LLVMDisposeBuilder(builder);
        return -1;
    }

    if (node->func.ret_type == TYPE_VOID) {
        LLVMBuildRetVoid(builder);
    } else if (ret_val) {
        LLVMBuildRet(builder, ret_val);
    } else {
        // return default zero value for non-void if missing
        LLVMBuildRet(builder, LLVMConstNull(ret_type));
    }

    symbol_table_free(&local_symtab);
    LLVMDisposeBuilder(builder);

    *func = function;
    return 0;
}
