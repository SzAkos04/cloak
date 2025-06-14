#include "codegen_return.h"

#include "ast.h"
#include "codegen_expr.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Types.h>

int codegen_return(ast_node_t *node, LLVMBuilderRef builder,
                   LLVMModuleRef module, LLVMContextRef context,
                   symbol_table_t *symtab, LLVMValueRef *ret) {
    if (node->type != AST_RETURN) {
        error("codegen_return called with non-return node");
        return -1;
    }

    if (node->return_stmt.value) {
        if (codegen_expression(node->return_stmt.value, builder, module,
                               context, symtab, ret) != 0) {
            return -1;
        }
    } else {
        *ret = NULL;
    }
    return 0;
}
