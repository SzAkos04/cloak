#include "codegen_utils.h"

#include "debug.h"

#include <llvm-c/Core.h>

LLVMTypeRef get_llvm_type(type_t type, LLVMContextRef context) {
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
