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
    symbol_table_t symtab = {0};
    symtab.head = NULL;

    bool main_found = false;

    // First pass: declare all functions
    for (int i = 0; i < ast->root->program.decl_count; ++i) {
        ast_node_t *decl = ast->root->program.decls[i];
        if (decl->type == AST_FUNCTION) {
            if (declare_fn(decl, *module, *context, &symtab) != 0) {
                symbol_table_free(&symtab);
                LLVMDisposeModule(*module);
                LLVMContextDispose(*context);
                return -1;
            }
        }
    }

    // Second pass: generate function bodies
    for (int i = 0; i < ast->root->program.decl_count; ++i) {
        ast_node_t *decl = ast->root->program.decls[i];
        if (decl->type == AST_FUNCTION) {
            LLVMValueRef func;
            if (codegen_fn(decl, *module, *context, &symtab, &func) != 0) {
                symbol_table_free(&symtab);
                LLVMDisposeModule(*module);
                LLVMContextDispose(*context);
                return -1;
            }

            if (strcmp(decl->func.name, "main") == 0) {
                main_found = true;
                if (decl->func.ret_type.kind != TYPE_PRIMARY ||
                    decl->func.ret_type.data.primary != TYPE_I32) {
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
        symbol_table_free(&symtab);
        LLVMDisposeModule(*module);
        LLVMContextDispose(*context);
        return -1;
    }

#ifdef DEBUG
    LLVMDumpModule(*module);
#endif

    symbol_table_free(&symtab);

    return 0;
}
