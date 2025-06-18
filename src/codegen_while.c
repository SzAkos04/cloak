#include "codegen_while.h"

#include "codegen_block.h"
#include "codegen_expr.h"
#include "debug.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int codegen_while(ast_node_t *node, LLVMBuilderRef builder,
                  LLVMModuleRef module, LLVMContextRef context,
                  symbol_table_t *symtab, LLVMValueRef *while_val) {
    if (node->type != AST_WHILE) {
        error("codegen_while called with non-while node");
        return -1;
    }

    LLVMValueRef function =
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));

    LLVMBasicBlockRef cond_bb =
        LLVMAppendBasicBlockInContext(context, function, "while.cond");
    LLVMBasicBlockRef body_bb =
        LLVMAppendBasicBlockInContext(context, function, "while.body");
    LLVMBasicBlockRef end_bb =
        LLVMAppendBasicBlockInContext(context, function, "while.end");

    LLVMBuildBr(builder, cond_bb);

    LLVMPositionBuilderAtEnd(builder, cond_bb);

    LLVMValueRef cond = NULL;
    if (codegen_expr(node->while_stmt.condition, builder, module, context,
                     symtab, &cond) != 0) {
        return -1;
    }

    // type check: condition must be i1 (boolean)
    LLVMTypeRef expected_type = LLVMInt1TypeInContext(context);
    LLVMTypeRef cond_type = LLVMTypeOf(cond);
    if (cond_type != expected_type) {
        char *expected_str = LLVMPrintTypeToString(expected_type);
        char *actual_str = LLVMPrintTypeToString(cond_type);
        error("type mismatch in while condition: expected `%s`, got `%s`",
              expected_str, actual_str);
        LLVMDisposeMessage(expected_str);
        LLVMDisposeMessage(actual_str);
        return -1;
    }

    LLVMBuildCondBr(builder, cond, body_bb, end_bb);

    LLVMPositionBuilderAtEnd(builder, body_bb);
    LLVMValueRef body_val = NULL;
    if (codegen_block(node->while_stmt.body, builder, module, context, symtab,
                      &body_val) != 0) {
        return -1;
    }

    // if body block doesn't have a terminator, branch back to condition
    if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder))) {
        LLVMBuildBr(builder, cond_bb);
    }

    LLVMPositionBuilderAtEnd(builder, end_bb);

    *while_val = NULL; // while loops do not produce values

    return 0;
}
