#include "codegen_return.h"

#include "ast.h"
#include "codegen_expr.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int codegen_return(ast_node_t *node, LLVMBuilderRef builder,
                   LLVMModuleRef module, LLVMContextRef context,
                   symbol_table_t *symtab, LLVMValueRef *ret) {
    if (node->type != AST_RETURN) {
        error("codegen_return called with non-return node");
        return -1;
    }

    LLVMValueRef current_func =
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMTypeRef expected_ret_type =
        LLVMGetReturnType(LLVMGlobalGetValueType(current_func));

    if (node->return_stmt.value) {
        if (codegen_expression(node->return_stmt.value, builder, module,
                               context, symtab, ret) != 0) {
            return -1;
        }

        LLVMTypeRef ret_type = LLVMTypeOf(*ret);
        // check for type mismatch
        if (ret_type != expected_ret_type) {
            const char *func_name = LLVMGetValueName(current_func);
            char *expected_str = LLVMPrintTypeToString(expected_ret_type);
            char *actual_str = LLVMPrintTypeToString(ret_type);
            error("return type mismatch for function `%s`: expected `%s`, got "
                  "`%s`",
                  func_name, expected_str, actual_str);
            LLVMDisposeMessage(expected_str);
            LLVMDisposeMessage(actual_str);
            return -1;
        }
    } else {
        *ret = NULL;
    }
    return 0;
}
