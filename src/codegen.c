#include "codegen.h"

#include "ast.h"
#include "debug.h"

#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Types.h>

static int codegen_expression(ast_node_t *node, LLVMBuilderRef builder,
                              LLVMModuleRef module, LLVMContextRef context,
                              LLVMValueRef *expr) {
    (void)module;
    switch (node->type) {
    case AST_LITERAL:
        switch (node->literal.kind) {
        case LITERAL_NUMBER:
            switch (node->literal.num_type) {
            case TYPE_F32:
                *expr = LLVMConstReal(LLVMFloatTypeInContext(context),
                                      node->literal.number);
                return 0;
            case TYPE_F64:
                *expr = LLVMConstReal(LLVMDoubleTypeInContext(context),
                                      node->literal.number);
                return 0;
            case TYPE_I8:
                *expr = LLVMConstInt(LLVMInt8TypeInContext(context),
                                     (uint64_t)node->literal.number, true);
                return 0;
            case TYPE_I16:
                *expr = LLVMConstInt(LLVMInt16TypeInContext(context),
                                     (uint64_t)node->literal.number, true);
                return 0;
            case TYPE_I32:
                *expr = LLVMConstInt(LLVMInt32TypeInContext(context),
                                     (uint64_t)node->literal.number, true);
                return 0;
            case TYPE_I64:
                *expr = LLVMConstInt(LLVMInt64TypeInContext(context),
                                     (uint64_t)node->literal.number, true);
                return 0;
            default:
                error("incorrect number type, found %s",
                      TYPE_TO_STR(node->literal.num_type));
                return -1;
            }
        case LITERAL_STRING: {
            static int str_counter = 0;
            char name[16];
            snprintf(name, sizeof(name), "str.%d", str_counter++);

            *expr =
                LLVMBuildGlobalStringPtr(builder, node->literal.string, name);
            return 0;
        }
        case LITERAL_BOOL:
            *expr = LLVMConstInt(LLVMInt1TypeInContext(context),
                                 node->literal.boolean, false);
            return 0;
        }
        break;
    case AST_IDENTIFIER:
        // Lookup variable value from a symbol table (you will need to implement
        // this) For now, return NULL or error
        error("identifier codegen not yet implemented");
        return -1;
    default:
        error("unknown expression node type");
        return -1;
    }
    return -1;
}

static int codegen_return(ast_node_t *node, LLVMBuilderRef builder,
                          LLVMModuleRef module, LLVMContextRef context,
                          LLVMValueRef *ret) {
    LLVMValueRef ret_val = NULL;
    if (node->return_stmt.value) {
        if (codegen_expression(node->return_stmt.value, builder, module,
                               context, &ret_val) != 0) {
            return -1;
        }
    }
    *ret = LLVMBuildRet(builder, ret_val);
    return 0;
}

static int codegen_block(ast_node_t *node, LLVMBuilderRef builder,
                         LLVMModuleRef module, LLVMContextRef context,
                         LLVMValueRef *block) {
    LLVMValueRef last = NULL;
    for (int i = 0; i < node->block.stmt_count; i++) {
        ast_node_t *stmt = node->block.stmt[i];
        if (stmt->type == AST_RETURN) {
            if (codegen_return(stmt, builder, module, context, &last) != 0) {
                return -1;
            }
            // return terminates the block early
            break;
        }
        // handle other statement types...
    }
    *block = last;
    return 0;
}

static int codegen_function(ast_node_t *fn, LLVMModuleRef module,
                            LLVMContextRef context, LLVMValueRef *func) {
    // Function signature: no params, returning double for example
    LLVMTypeRef ret_type;
    switch (fn->func.ret_type) {
    case TYPE_BOOL:
        ret_type = LLVMInt1TypeInContext(context);
        break;
    case TYPE_F32:
        ret_type = LLVMFloatTypeInContext(context);
        break;
    case TYPE_F64:
        ret_type = LLVMDoubleTypeInContext(context);
        break;
    case TYPE_I8:
        ret_type = LLVMInt8TypeInContext(context);
        break;
    case TYPE_I16:
        ret_type = LLVMInt16TypeInContext(context);
        break;
    case TYPE_I32:
        ret_type = LLVMInt32TypeInContext(context);
        break;
    case TYPE_I64:
        ret_type = LLVMInt64TypeInContext(context);
        break;
    case TYPE_STRING:
        ret_type = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
        break;
    case TYPE_VOID:
        ret_type = LLVMVoidTypeInContext(context);
        break;
    }
    LLVMTypeRef func_type = LLVMFunctionType(ret_type, NULL, 0, false);

    LLVMValueRef function = LLVMAddFunction(module, fn->func.name, func_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(function, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);

    LLVMValueRef ret_val;
    if (codegen_block(fn->func.body, builder, module, context, &ret_val) != 0) {
        return -1;
    }

    if (fn->func.ret_type == TYPE_VOID) {
        LLVMBuildRetVoid(builder);
    } else if (ret_val) {
        LLVMBuildRet(builder, ret_val);
    } else {
        // Return default zero value for non-void if missing
        LLVMBuildRet(builder, LLVMConstNull(ret_type));
    }

    LLVMDisposeBuilder(builder);

    *func = function;
    return 0;
}

int gen_IR(ast_t *ast, LLVMContextRef *context, LLVMModuleRef *module) {
    if (ast->root->type != AST_FUNCTION) {
        error("Top-level node type not supported for codegen");
        return -1;
    }

    if (strcmp(ast->root->func.name, "main") != 0) {
        error("Program must have a 'main' function");
        return -1;
    }

    if (ast->root->func.ret_type != TYPE_I32) {
        error("'main' function must return i32");
        return -1;
    }

    *context = LLVMContextCreate();
    *module = LLVMModuleCreateWithNameInContext("main_module", *context);

    LLVMValueRef func;
    if (codegen_function(ast->root, *module, *context, &func) != 0) {
        LLVMDisposeModule(*module);
        LLVMContextDispose(*context);
        return -1;
    }

    return 0;
}
