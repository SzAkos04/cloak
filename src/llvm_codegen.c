#include "llvm_codegen.h"

#include "debug.h"

#include <stdio.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

static void initialize_llvm_targets() {
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
}

static int create_target_machine(LLVMTargetMachineRef *target_machine) {
    char *err = NULL;
    LLVMTargetRef target;
    char *triple = LLVMGetDefaultTargetTriple();

    if (LLVMGetTargetFromTriple(triple, &target, &err) != 0) {
        error("failed to get target from triple: %s", err);
        LLVMDisposeMessage(err);
        return -1;
    }

    const char *cpu = "generic";
    const char *features = "";

    *target_machine = LLVMCreateTargetMachine(
        target, triple, cpu, features, LLVMCodeGenLevelDefault,
        LLVMRelocDefault, LLVMCodeModelDefault);

    LLVMDisposeMessage(triple);

    if (!*target_machine) {
        error("failed to create target machine");
        return -1;
    }

    return 0;
}

static int emit_object_file(LLVMModuleRef module,
                            LLVMTargetMachineRef target_machine,
                            const char *filename) {
    char *err = NULL;
    if (LLVMTargetMachineEmitToFile(target_machine, module, (char *)filename,
                                    LLVMObjectFile, &err) != 0) {
        error("failed to emit object file: %s", err);
        LLVMDisposeMessage(err);
        return -1;
    }
    return 0;
}

int gen_machine_code(LLVMModuleRef module, const char *output_filename) {
    initialize_llvm_targets();

    LLVMTargetMachineRef target_machine;
    if (create_target_machine(&target_machine) != 0) {
        return -1;
    }

    if (emit_object_file(module, target_machine, output_filename) != 0) {
        return -1;
    }

    LLVMDisposeTargetMachine(target_machine);

    return 0;
}
