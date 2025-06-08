#pragma once

#include <llvm-c/Types.h>

int gen_machine_code(LLVMModuleRef module, const char *output_filename);
