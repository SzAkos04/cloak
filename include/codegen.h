#pragma once

#include "ast.h"

#include <llvm-c/Types.h>

int gen_IR(ast_t *ast, LLVMContextRef *context, LLVMModuleRef *module);
