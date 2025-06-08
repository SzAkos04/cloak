#include "codegen.h"

#include "ast.h"
#include "debug.h"
#include "symbol_table.h"

#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Types.h>

static LLVMTypeRef get_llvm_type(type_t type, LLVMContextRef context) {
    switch (type) {
    case TYPE_BOOL:
        return LLVMInt1TypeInContext(context);
    case TYPE_F32:
        return LLVMFloatTypeInContext(context);
    case TYPE_F64:
        return LLVMDoubleTypeInContext(context);
    case TYPE_I8:
        return LLVMInt8TypeInContext(context);
    case TYPE_I16:
        return LLVMInt16TypeInContext(context);
    case TYPE_I32:
        return LLVMInt32TypeInContext(context);
    case TYPE_I64:
        return LLVMInt64TypeInContext(context);
    case TYPE_STRING:
        return LLVMPointerType(LLVMInt8TypeInContext(context), 0);
    case TYPE_VOID:
        return LLVMVoidTypeInContext(context);
    default:
        error("unknown type");
        return NULL;
    }
}

static int codegen_expression(ast_node_t *node, LLVMBuilderRef builder,
                              LLVMModuleRef module, LLVMContextRef context,
                              symbol_table_t *symtab, LLVMValueRef *expr) {
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
    case AST_IDENTIFIER: {
        symbol_t *sym = symbol_table_lookup(symtab, node->identifier.name);
        if (!sym) {
            error("undefined variable `%s`", node->identifier.name);
            return -1;
        }
        *expr = LLVMBuildLoad2(
            builder,
            LLVMTypeOf(sym->value), // or get_llvm_type of the variable's type
            sym->value, node->identifier.name);
        char *str = LLVMPrintValueToString(sym->value);
        debug("%s", str);
        LLVMDisposeMessage(str);

        return 0;
    }
    default:
        error("unknown expression node type");
        return -1;
    }
    return -1;
}

static int codegen_let(ast_node_t *node, LLVMBuilderRef builder,
                       LLVMModuleRef module, LLVMContextRef context,
                       symbol_table_t *symtab, LLVMValueRef *let) {
    if (node->type != AST_LET) {
        error("codegen_let called with non-let node");
        return -1;
    }

    LLVMTypeRef var_type = get_llvm_type(node->let.type, context);
    if (!var_type) {
        return -1;
    }

    LLVMValueRef alloca_inst =
        LLVMBuildAlloca(builder, var_type, node->let.name);
    if (!alloca_inst) {
        error("codegen_let: failed to build alloca for %s", node->let.name);
        return -1;
    }

    LLVMValueRef init_val = NULL;
    if (node->let.value) {
        if (codegen_expression(node->let.value, builder, module, context,
                               symtab, &init_val) != 0) {
            return -1;
        }
    } else {
        init_val = LLVMGetUndef(var_type);
    }

    LLVMBuildStore(builder, init_val, alloca_inst);

    symbol_table_add(symtab, node->let.name, alloca_inst, node->let.is_mutable);

    *let = alloca_inst;

    return 0;
}

static int codegen_assign(ast_node_t *node, LLVMBuilderRef builder,
                          LLVMModuleRef module, LLVMContextRef context,
                          symbol_table_t *symtab, LLVMValueRef *assign) {
    if (node->type != AST_ASSIGN) {
        error("codegen_assign called with non-assign node");
        return -1;
    }

    // Lookup variable to assign to
    symbol_t *sym = symbol_table_lookup(symtab, node->assign.name);
    if (!sym) {
        error("undefined variable `%s`", node->assign.name);
        return -1;
    }

    if (!sym->is_mutable) {
        error("cannot mutate unmutable variable");
        return -1;
    }

    LLVMValueRef val;
    if (codegen_expression(node->assign.value, builder, module, context, symtab,
                           &val) != 0) {
        return -1;
    }

    LLVMBuildStore(builder, val, sym->value);

    if (assign) {
        *assign = val;
    }
    return 0;
}

static int codegen_return(ast_node_t *node, LLVMBuilderRef builder,
                          LLVMModuleRef module, LLVMContextRef context,
                          symbol_table_t *symtab, LLVMValueRef *ret) {
    if (node->type != AST_RETURN) {
        error("codegen_return called with non-return node");
        return -1;
    }

    LLVMValueRef ret_val = NULL;
    if (node->return_stmt.value) {
        if (codegen_expression(node->return_stmt.value, builder, module,
                               context, symtab, &ret_val) != 0) {
            return -1;
        }
    }
    *ret = LLVMBuildRet(builder, ret_val);
    return 0;
}

static int codegen_block(ast_node_t *node, LLVMBuilderRef builder,
                         LLVMModuleRef module, LLVMContextRef context,
                         symbol_table_t *symtab, LLVMValueRef *block) {
    if (node->type != AST_BLOCK) {
        error("codegen_block called with non-block node");
        return -1;
    }
    LLVMValueRef last = NULL;
    for (int i = 0; i < node->block.stmt_count; ++i) {
        ast_node_t *stmt = node->block.stmt[i];
        // debug("i=%d stmt_count=%d stmt ptr=%p\n", i, node->block.stmt_count,
        //       (void *)node->block.stmt[i]);
        if (stmt->type == AST_RETURN) {
            if (codegen_return(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
            // return terminates the block early
            break;
        } else if (stmt->type == AST_LET) {
            if (codegen_let(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else if (stmt->type == AST_ASSIGN) {
            if (codegen_assign(stmt, builder, module, context, symtab, &last) !=
                0) {
                return -1;
            }
        } else {
            error("not yet implemented");
            return -1;
        }
    }
    *block = last;
    return 0;
}

static int codegen_function(ast_node_t *node, LLVMModuleRef module,
                            LLVMContextRef context, symbol_table_t *symtab,
                            LLVMValueRef *func) {
    if (node->type != AST_FUNCTION) {
        error("codegen_function called with non-function node");
        return -1;
    }
    LLVMTypeRef ret_type;
    switch (node->func.ret_type) {
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

    LLVMValueRef function = LLVMAddFunction(module, node->func.name, func_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(function, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);

    LLVMValueRef ret_val;
    if (codegen_block(node->func.body, builder, module, context, symtab,
                      &ret_val) != 0) {
        return -1;
    }

    if (node->func.ret_type == TYPE_VOID) {
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
    symbol_table_t symtab;
    symtab.head = NULL;

    LLVMValueRef func;
    if (codegen_function(ast->root, *module, *context, &symtab, &func) != 0) {
        LLVMDisposeModule(*module);
        LLVMContextDispose(*context);
        symbol_table_free(&symtab);
        return -1;
    }

#ifdef DEBUG
    LLVMDumpModule(*module);
#endif

    symbol_table_free(&symtab);

    return 0;
}
