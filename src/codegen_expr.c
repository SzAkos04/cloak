#include "codegen_expr.h"

#include "ast.h"
#include "codegen_utils.h"
#include "debug.h"
#include "symbol_table.h"
#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <stdlib.h>

int codegen_expression(ast_node_t *node, LLVMBuilderRef builder,
                       LLVMModuleRef module, LLVMContextRef context,
                       symbol_table_t *symtab, LLVMValueRef *expr) {
    (void)module;
    switch (node->type) {
    case AST_LITERAL:
        switch (node->literal.kind) {
        case LITERAL_NUMBER:
            switch (node->literal.num_type) {
            case TYPE_F32:
            case TYPE_F64:
                *expr = LLVMConstReal(
                    get_llvm_type(node->literal.num_type, context),
                    node->literal.number);
                return 0;
            case TYPE_I8:
            case TYPE_I16:
            case TYPE_I32:
            case TYPE_I64:
                *expr =
                    LLVMConstInt(get_llvm_type(node->literal.num_type, context),
                                 (uint64_t)node->literal.number, true);
                return 0;
            default:
                error("incorrect number type, found %s",
                      type_to_str(node->literal.num_type));
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
        *expr = LLVMBuildLoad2(builder, sym->type, sym->value,
                               node->identifier.name);

        return 0;
    }

    case AST_UNARY: {
        LLVMValueRef operand;
        if (codegen_expression(node->unary.right, builder, module, context,
                               symtab, &operand) != 0) {
            return -1;
        }

        switch (node->unary.op) {
        case UNARY_NEGATE: {
            if (LLVMGetTypeKind(LLVMTypeOf(operand)) == LLVMFloatTypeKind ||
                LLVMGetTypeKind(LLVMTypeOf(operand)) == LLVMDoubleTypeKind) {
                *expr = LLVMBuildFNeg(builder, operand, "negtmp");
            } else {
                *expr = LLVMBuildNeg(builder, operand, "negtmp");
            }
            return 0;
        }
        case UNARY_NOT: {
            *expr = LLVMBuildNot(builder, operand, "nottmp");
            return 0;
        }
        }
    } break;

    case AST_BINARY: {
        LLVMValueRef lhs, rhs;
        if (codegen_expression(node->binary.left, builder, module, context,
                               symtab, &lhs) != 0) {
            return -1;
        }
        if (codegen_expression(node->binary.right, builder, module, context,
                               symtab, &rhs) != 0) {
            return -1;
        }

        LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
        // LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

        bool is_float =
            (lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind);

        // TODO: logical operators
        switch (node->binary.op) {
        case BIN_ADD:
            *expr = is_float ? LLVMBuildFAdd(builder, lhs, rhs, "addtmp")
                             : LLVMBuildAdd(builder, lhs, rhs, "addtmp");
            return 0;
        case BIN_SUB:
            *expr = is_float ? LLVMBuildFSub(builder, lhs, rhs, "subtmp")
                             : LLVMBuildSub(builder, lhs, rhs, "subtmp");
            return 0;
        case BIN_MUL:
            *expr = is_float ? LLVMBuildFMul(builder, lhs, rhs, "multmp")
                             : LLVMBuildMul(builder, lhs, rhs, "multmp");
            return 0;
        case BIN_DIV:
            *expr = is_float ? LLVMBuildFDiv(builder, lhs, rhs, "divtmp")
                             : LLVMBuildSDiv(builder, lhs, rhs,
                                             "divtmp"); // assumes signed ints
            return 0;
        case BIN_MOD:
            *expr = is_float ? LLVMBuildFRem(builder, lhs, rhs, "modtmp")
                             : LLVMBuildSRem(builder, lhs, rhs, "modtmp");
            return 0;

        case BIN_EQ:
            *expr = is_float
                        ? LLVMBuildFCmp(builder, LLVMRealOEQ, lhs, rhs, "eqtmp")
                        : LLVMBuildICmp(builder, LLVMIntEQ, lhs, rhs, "eqtmp");
            return 0;
        case BIN_NEQ:
            *expr = is_float
                        ? LLVMBuildFCmp(builder, LLVMRealONE, lhs, rhs, "netmp")
                        : LLVMBuildICmp(builder, LLVMIntNE, lhs, rhs, "netmp");
            return 0;

        case BIN_LT:
            *expr =
                is_float
                    ? LLVMBuildFCmp(builder, LLVMRealOLT, lhs, rhs, "cmptmp")
                    : LLVMBuildICmp(builder, LLVMIntSLT, lhs, rhs, "cmptmp");
            return 0;
        case BIN_GT:
            *expr =
                is_float
                    ? LLVMBuildFCmp(builder, LLVMRealOGT, lhs, rhs, "cmptmp")
                    : LLVMBuildICmp(builder, LLVMIntSGT, lhs, rhs, "cmptmp");
            return 0;
        default:
            error("unsupported binary operator `%s`",
                  binary_op_to_str(node->binary.op));
            return -1;
        }
    } break;
    case AST_CALL: {
        LLVMValueRef callee = LLVMGetNamedFunction(module, node->call.name);
        if (!callee) {
            error("undefined function `%s`", node->call.name);
            return -1;
        }

        int arg_count = node->call.param_count;
        LLVMValueRef *args = NULL;

        if (arg_count > 0) {
            args = (LLVMValueRef *)malloc(sizeof(LLVMValueRef) * arg_count);
            if (!args) {
                perr("codegen: failed to allocate memory for call arguments");
                return -1;
            }
        }

        for (int i = 0; i < arg_count; ++i) {
            if (codegen_expression(node->call.args[i], builder, module, context,
                                   symtab, &args[i]) != 0) {
                free(args);
                return -1;
            }
        }

        LLVMTypeRef callee_type = LLVMTypeOf(callee);
        if (!callee_type) {
            error("codegen: failed to get type of callee");
            free(args);
            return -1;
        }

        if (LLVMGetTypeKind(callee_type) != LLVMPointerTypeKind) {
            error("codegen: expected a function pointer type, got %d",
                  LLVMGetTypeKind(callee_type));
            free(args);
            return -1;
        }

        LLVMTypeRef func_type = LLVMGetElementType(callee_type);
        if (!func_type) {
            error("codegen: failed to get type of function");
            free(args);
            return -1;
        }

        if (LLVMGetTypeKind(func_type) != LLVMFunctionTypeKind) {
            error("codegen: expected a function type, got %d",
                  LLVMGetTypeKind(func_type));
            free(args);
            return -1;
        }

        *expr = LLVMBuildCall2(builder, func_type, callee, args, arg_count,
                               "calltmp");
        free(args);
        return 0;
    }
    default:
        error("unknown expression node type");
        return -1;
    }
    return -1;
}
