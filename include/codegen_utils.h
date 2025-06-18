#pragma once

#include "ast.h"

#include <llvm-c/Types.h>

LLVMTypeRef get_llvm_primary_type(primary_type_t type, LLVMContextRef context);
LLVMTypeRef get_llvm_type(type_t type, LLVMContextRef context);
