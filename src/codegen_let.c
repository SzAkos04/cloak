#include "codegen_let.h"

#include "ast.h"
#include "codegen_expr.h"
#include "codegen_utils.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int codegen_let(ast_node_t *node, LLVMBuilderRef builder, LLVMModuleRef module,
                LLVMContextRef context, symbol_table_t *symtab,
                LLVMValueRef *let) {
    if (node->type != AST_LET) {
        error("codegen_let called with non-let node");
        return -1;
    }

    LLVMTypeRef var_type = get_llvm_type(node->let.type, context);
    if (!var_type) {
        return -1;
    }

    LLVMValueRef alloca_inst =
        LLVMBuildAlloca(builder, var_type, node->let.name);
    if (!alloca_inst) {
        error("codegen_let: failed to build alloca for %s", node->let.name);
        return -1;
    }

    LLVMValueRef init_val = NULL;
    if (node->let.value) {
        if (codegen_expression(node->let.value, builder, module, context,
                               symtab, &init_val) != 0) {
            return -1;
        }
    } else {
        init_val = LLVMGetUndef(var_type);
    }

    LLVMBuildStore(builder, init_val, alloca_inst);

    symbol_table_add(symtab, node->let.name, var_type, alloca_inst,
                     node->let.is_mutable);

    *let = alloca_inst;

    return 0;
}
