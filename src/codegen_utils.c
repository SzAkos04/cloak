#include "codegen_utils.h"

#include "ast.h"
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
    if (type.kind == TYPE_PRIMARY) {
        return get_llvm_primary_type(type.data.primary, context);
    } else {
        LLVMTypeRef element_type =
            get_llvm_type(*type.data.array.type, context);
        if (type.data.array.length > 0) {
            // Fixed-size array
            return LLVMArrayType(element_type,
                                 (unsigned)type.data.array.length);
        } else {
            // Dynamic array as pointer to element
            return LLVMPointerType(element_type, 0);
        }
    }
}
