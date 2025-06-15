#include "codegen_if.h"

#include "ast.h"
#include "codegen_block.h"
#include "codegen_expr.h"
#include "debug.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int codegen_if(ast_node_t *node, LLVMBuilderRef builder, LLVMModuleRef module,
               LLVMContextRef context, symbol_table_t *symtab,
               LLVMValueRef *if_val) {
    if (node->type != AST_IF) {
        error("codegen_if called with non-if node");
        return -1;
    }

    LLVMValueRef cond = NULL;
    if (codegen_expression(node->if_stmt.condition, builder, module, context,
                           symtab, &cond) != 0) {
        return -1;
    }

    // type check
    LLVMTypeRef expected_type = LLVMInt1TypeInContext(context);
    LLVMTypeRef cond_type = LLVMTypeOf(cond);
    if (cond_type != expected_type) {
        char *expected_str = LLVMPrintTypeToString(expected_type);
        char *actual_str = LLVMPrintTypeToString(cond_type);
        error("type mismatch in if statement: expected `%s`, got `%s`",
              expected_str, actual_str);
        LLVMDisposeMessage(expected_str);
        LLVMDisposeMessage(actual_str);
        return -1;
    }

    LLVMValueRef function =
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMBasicBlockRef then_bb =
        LLVMAppendBasicBlockInContext(context, function, "then");
    LLVMBasicBlockRef else_bb = NULL;
    LLVMBasicBlockRef end_bb =
        LLVMAppendBasicBlockInContext(context, function, "endif");

    if (node->if_stmt.else_block) {
        else_bb = LLVMAppendBasicBlockInContext(context, function, "else");
        // conditionally branch to then_bb or else_bb
        LLVMBuildCondBr(builder, cond, then_bb, else_bb);
    } else {
        // no else block, branch to then or end directly
        LLVMBuildCondBr(builder, cond, then_bb, end_bb);
    }

    LLVMPositionBuilderAtEnd(builder, then_bb);
    LLVMValueRef then_val = NULL;
    if (codegen_block(node->if_stmt.then_block, builder, module, context,
                      symtab, &then_val) != 0) {
        return -1;
    }
    if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder))) {
        LLVMBuildBr(builder, end_bb);
    }

    if (else_bb) {
        LLVMPositionBuilderAtEnd(builder, else_bb);
        LLVMValueRef else_val = NULL;
        if (codegen_block(node->if_stmt.else_block, builder, module, context,
                          symtab, &else_val) != 0) {
            return -1;
        }
        if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder))) {
            LLVMBuildBr(builder, end_bb);
        }
    }

    LLVMPositionBuilderAtEnd(builder, end_bb);

    *if_val = NULL; // if statements don't produce values

    return 0;
}
