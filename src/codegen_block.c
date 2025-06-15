#include "codegen_block.h"

#include "ast.h"
#include "codegen_assign.h"
#include "codegen_if.h"
#include "codegen_let.h"
#include "codegen_return.h"
#include "codegen_while.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Types.h>

int codegen_block(ast_node_t *node, LLVMBuilderRef builder,
                  LLVMModuleRef module, LLVMContextRef context,
                  symbol_table_t *symtab, LLVMValueRef *block) {
    if (node->type != AST_BLOCK) {
        error("codegen_block called with non-block node");
        return -1;
    }
    LLVMValueRef last = NULL;
    for (int i = 0; i < node->block.stmt_count; ++i) {
        ast_node_t *stmt = node->block.stmt[i];
        if (stmt->type == AST_IF) {
            if (codegen_if(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else if (stmt->type == AST_LET) {
            if (codegen_let(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else if (stmt->type == AST_RETURN) {
            if (codegen_return(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
            break;
        } else if (stmt->type == AST_WHILE) {
            if (codegen_while(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else if (stmt->type == AST_ASSIGN) {
            if (codegen_assign(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else {
            error("not yet implemented");
            return -1;
        }
    }
    *block = last;
    return 0;
}
