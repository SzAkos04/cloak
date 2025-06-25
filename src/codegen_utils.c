#include "codegen_utils.h"

#include "ast.h"
#include "codegen_const_expr.h"
#include "debug.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

LLVMTypeRef get_llvm_primary_type(primary_type_t type, LLVMContextRef context) {
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

LLVMTypeRef get_llvm_type(type_t type, LLVMContextRef context) {
    switch (type.kind) {
    case TYPE_PRIMARY:
        return get_llvm_primary_type(type.data.primary, context);
    case TYPE_ARRAY: {
        LLVMTypeRef element_type =
            get_llvm_type(*type.data.array.type, context);
        int len = 0;
        if (evaluate_constant_expr(type.data.array.len, &len) != 0) {
            return NULL;
        }
        if (len > 0) {
            // Fixed-size array
            return LLVMArrayType(element_type, (unsigned)len);
        } else {
            // Dynamic array as pointer to element
            error("array length must be greater than 0");
            return NULL;
        }
    }
    default:
        error("unknown type");
        return NULL;
    }
}
