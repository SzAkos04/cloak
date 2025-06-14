#pragma once

#include "ast.h"
#include "symbol_table.h"

#include <llvm-c/Types.h>

int codegen_fn(ast_node_t *node, LLVMModuleRef module, LLVMContextRef context,
               symbol_table_t *symtab, LLVMValueRef *func);
