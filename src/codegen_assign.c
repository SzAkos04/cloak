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

    // lookup variable to assign to
    symbol_t *sym = symbol_table_lookup(symtab, node->assign.name);
    if (!sym) {
        error("undefined variable `%s`", node->assign.name);
        return -1;
    }

    if (!sym->is_mutable) {
        error("cannot mutate unmutable variable `%s`", sym->name);
        return -1;
    }

    LLVMValueRef val;
    if (codegen_expression(node->assign.value, builder, module, context, symtab,
                           &val) != 0) {
        return -1;
    }

    LLVMBuildStore(builder, val, sym->value);

    if (assign) {
        *assign = val;
    }
    return 0;
}
