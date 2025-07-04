#pragma once

#include "ast.h"
#include "symbol_table.h"

#include <llvm-c/Types.h>

int codegen_expr(ast_node_t *node, LLVMBuilderRef builder, LLVMModuleRef module,
                 LLVMContextRef context, symbol_table_t *symtab,
                 LLVMValueRef *expr);
