#include "codegen.h"

#include "ast.h"
#include "codegen_fn.h"
#include "debug.h"
#include "symbol_table.h"

#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Types.h>

int gen_IR(ast_t *ast, LLVMContextRef *context, LLVMModuleRef *module) {
    if (ast->root->type != AST_PROGRAM) {
        error("top-level node type not supported for codegen");
        return -1;
    }

    *context = LLVMContextCreate();
    *module = LLVMModuleCreateWithNameInContext("main_module", *context);
    symbol_table_t symtab;
    symtab.head = NULL;

    LLVMValueRef func;
    bool main_found = false;
    for (int i = 0; i < ast->root->program.decl_count; ++i) {
        if (ast->root->program.decls[i]->type == AST_FUNCTION) {
            if (codegen_fn(ast->root->program.decls[i], *module, *context,
                           &symtab, &func) != 0) {
                symbol_table_free(&symtab);
                LLVMDisposeModule(*module);
                LLVMContextDispose(*context);
                return -1;
            }

            if (strcmp(ast->root->program.decls[i]->func.name, "main") == 0) {
                main_found = true;

                if (ast->root->program.decls[i]->func.ret_type != TYPE_I32) {
                    error("`main` function must return i32");
                    symbol_table_free(&symtab);
                    LLVMDisposeModule(*module);
                    LLVMContextDispose(*context);
                    return -1;
                }
            }
        }
    }

    if (!main_found) {
        error("program must have a `main` function");
        return -1;
    }

#ifdef DEBUG
    LLVMDumpModule(*module);
#endif

    symbol_table_free(&symtab);

    return 0;
}
