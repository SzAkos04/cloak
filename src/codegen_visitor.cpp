#include "codegen_visitor.hpp"

#include "ast.hpp"
#include "cli.hpp"
#include "type.hpp"

#include <fmt/core.h>
#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>
#include <memory>
#include <string>
#include <system_error>

void CodegenVisitor::visit(AstProgram &node) {
    this->declareGlobals(node);

    for (const auto &decl : node.decls) {
        if (auto *fn = dynamic_cast<AstFn *>(decl.get())) {
            fn->accept(*this);
        } else if ([[maybe_unused]] auto *let =
                       dynamic_cast<AstLet *>(decl.get())) {
            continue;
        } else {
            THROW_CODEGEN("Top-level non-function declarations not supported",
                          this->verbose);
        }
    }

    // verify the IR
    std::string errStr;
    llvm::raw_string_ostream errStream(errStr);

    if (llvm::verifyModule(*this->module, &errStream)) {
        this->dumpIR();
        THROW_CODEGEN(
            fmt::format("IR verification failed:\n{}", errStream.str()),
            this->verbose);
    }

    this->optimize();

    this->emitObjectFile(this->filename);
}

void CodegenVisitor::visit(AstIdentifier &node) {
    auto it = this->symbolTable.find(node.name);
    if (it == this->symbolTable.end()) {
        THROW_CODEGEN(
            fmt::format("Use of undefined identifier `{}`", node.name),
            this->verbose);
    }

    VariableInfo &info = it->second;
    llvm::Value *ptr = info.value;
    llvm::Type *ptrTy = toLLVMType(*info.type);

    node.inferredType = info.type.get();
    this->lastValue = this->builder.CreateLoad(ptrTy, ptr, "loadtmp");
}

void CodegenVisitor::visit(AstLiteral &node) {
    const auto &val = node.value;

    if (val.isNumber()) {
        auto number = val.getNumber();
        llvm::Type *ty = nullptr;
        llvm::Value *constVal = nullptr;

        switch (number.type) {
        case PrimaryType::I8:
        case PrimaryType::I16:
        case PrimaryType::I32:
        case PrimaryType::I64:
        case PrimaryType::U8:
        case PrimaryType::U16:
        case PrimaryType::U32:
        case PrimaryType::U64: {
            ty = toLLVMType(Type(number.type));
            constVal = llvm::ConstantInt::get(ty, number.value, false);
            static Type tempType(number.type);
            node.inferredType = &tempType;
            break;
        }

        case PrimaryType::F32: {
            ty = llvm::Type::getFloatTy(*this->context);
            constVal = llvm::ConstantFP::get(ty, number.value);
            static Type tempType(PrimaryType::F32);
            node.inferredType = &tempType;
            break;
        }

        case PrimaryType::F64: {
            ty = llvm::Type::getDoubleTy(*this->context);
            constVal = llvm::ConstantFP::get(ty, number.value);
            static Type tempType(PrimaryType::F64);
            node.inferredType = &tempType;
            break;
        }

        default:
            THROW_CODEGEN("Unsupported literal numeric type", this->verbose);
        }

        lastValue = constVal;
    } else if (val.isBool()) {
        static Type tempType(PrimaryType::Bool);
        node.inferredType = &tempType;
        lastValue = llvm::ConstantInt::get(
            llvm::Type::getInt1Ty(*this->context), val.getBool() ? 1 : 0);
    } else if (val.isString()) {
        const std::string &str = val.getString();
        llvm::Constant *strConstant =
            llvm::ConstantDataArray::getString(*this->context, str);
        llvm::GlobalVariable *strVar = new llvm::GlobalVariable(
            *this->module, strConstant->getType(), true,
            llvm::GlobalValue::PrivateLinkage, strConstant, ".str");

        static Type tempType(PrimaryType::String);
        node.inferredType = &tempType;
        lastValue = this->builder.CreatePointerCast(
            strVar, llvm::PointerType::getInt8Ty(*this->context));
    } else {
        THROW_CODEGEN("Unsupported literal type", this->verbose);
    }
}

void CodegenVisitor::visit(AstBlock &node) {
    for (const auto &stmt : node.stmts) {
        stmt->accept(*this);
    }
}

void CodegenVisitor::visit(AstUnary &node) {
    node.rhs->accept(*this);
    llvm::Value *operand = lastValue;

    const Type *operandType = nullptr;
    if (auto *idNode = dynamic_cast<AstIdentifier *>(node.rhs.get())) {
        operandType = idNode->inferredType;
    } else if (auto *litNode = dynamic_cast<AstLiteral *>(node.rhs.get())) {
        operandType = litNode->inferredType;
    } else {
        THROW_CODEGEN("Unable to infer type from RHS of unary expression",
                      this->verbose);
    }

    switch (node.op) {
    case UnaryOp::Negate:
        if (operand->getType()->isFloatingPointTy() ||
            operand->getType()->isIntegerTy()) {
            lastValue = operand->getType()->isFloatingPointTy()
                            ? this->builder.CreateFNeg(operand, "negtmp")
                            : this->builder.CreateNeg(operand, "negtmp");
            node.inferredType = operandType;
        } else {
            THROW_CODEGEN("Invalid type for unary negate", this->verbose);
        }
        break;

    case UnaryOp::Not:
        lastValue = this->builder.CreateNot(operand, "nottmp");
        static Type tempType(PrimaryType::Bool);
        node.inferredType = &tempType;
        break;

    default:
        THROW_CODEGEN("Unsupported unary operator", this->verbose);
    }
}

void CodegenVisitor::visit(AstBinary &node) {
    node.lhs->accept(*this);
    const Type *lhsType = node.lhs->inferredType;
    llvm::Value *left = lastValue;

    node.rhs->accept(*this);
    llvm::Value *right = lastValue;

    bool isFloat = left->getType()->isFloatingPointTy();
    bool isSigned = lhsType->isSigned();

    switch (node.op) {
    case BinaryOp::Add:
        lastValue = isFloat ? this->builder.CreateFAdd(left, right, "addtmp")
                            : this->builder.CreateAdd(left, right, "addtmp");
        break;
    case BinaryOp::Sub:
        lastValue = isFloat ? this->builder.CreateFSub(left, right, "subtmp")
                            : this->builder.CreateSub(left, right, "subtmp");
        break;
    case BinaryOp::Mul:
        lastValue = isFloat ? this->builder.CreateFMul(left, right, "multmp")
                            : this->builder.CreateMul(left, right, "multmp");
        break;
    case BinaryOp::Div:
        if (isFloat) {
            lastValue = this->builder.CreateFDiv(left, right, "divtmp");
        } else {
            lastValue = isSigned
                            ? this->builder.CreateSDiv(left, right, "sdivtmp")
                            : this->builder.CreateUDiv(left, right, "udivtmp");
        }
        break;

    case BinaryOp::Eq:
        lastValue = isFloat ? this->builder.CreateFCmpUEQ(left, right, "eqtmp")
                            : this->builder.CreateICmpEQ(left, right, "eqtmp");
        break;
    case BinaryOp::Neq:
        lastValue = isFloat ? this->builder.CreateFCmpUNE(left, right, "neqtmp")
                            : this->builder.CreateICmpNE(left, right, "neqtmp");
        break;
    case BinaryOp::Lt:
        lastValue = isFloat ? this->builder.CreateFCmpULT(left, right, "lttmp")
                    : isSigned
                        ? this->builder.CreateICmpSLT(left, right, "lttmp")
                        : this->builder.CreateICmpULT(left, right, "lttmp");
        break;
    case BinaryOp::Lte:
        lastValue = isFloat ? this->builder.CreateFCmpULE(left, right, "ltetmp")
                    : isSigned
                        ? this->builder.CreateICmpSLE(left, right, "ltetmp")
                        : this->builder.CreateICmpULE(left, right, "ltetmp");
        break;
    case BinaryOp::Gt:
        lastValue = isFloat ? this->builder.CreateFCmpUGT(left, right, "gttmp")
                    : isSigned
                        ? this->builder.CreateICmpSGT(left, right, "gttmp")
                        : this->builder.CreateICmpUGT(left, right, "gttmp");
        break;
    case BinaryOp::Gte:
        lastValue = isFloat ? this->builder.CreateFCmpUGE(left, right, "gtetmp")
                    : isSigned
                        ? this->builder.CreateICmpSGE(left, right, "gtetmp")
                        : this->builder.CreateICmpUGE(left, right, "gtetmp");
        break;

    default:
        THROW_CODEGEN("Unsupported binary operator", this->verbose);
    }

    if (node.op == BinaryOp::Eq || node.op == BinaryOp::Neq ||
        node.op == BinaryOp::Lt || node.op == BinaryOp::Lte ||
        node.op == BinaryOp::Gt || node.op == BinaryOp::Gte) {
        static Type tempType(PrimaryType::Bool);
        node.inferredType = &tempType;
    } else {
        node.inferredType = lhsType;
    }
}

void CodegenVisitor::visit(AstFn &node) {
    // retrieve the function prototype from the module
    llvm::Function *func = this->module->getFunction(node.name);
    if (!func) {
        THROW_CODEGEN(fmt::format("Use of undeclared function `{}`", node.name),
                      this->verbose);
    }

    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*this->context, "entry", func);
    this->builder.SetInsertPoint(entry);

    // register parameters in symbol table
    unsigned idx = 0;
    for (auto &arg : func->args()) {
        const std::string &pname = node.params[idx].name;
        arg.setName(pname);
        this->declareSymbol(pname, &node.params[idx].type, /*mut=*/false, &arg);
        ++idx;
    }

    node.body->accept(*this);

    // if no terminator, auto-generate
    if (!this->builder.GetInsertBlock()->getTerminator()) {
        if (node.retType.kind == Type::Kind::Primary &&
            std::get<PrimaryType>(node.retType.data) == PrimaryType::Void) {
            this->builder.CreateRetVoid();
        } else {
            llvm::Type *retTy = toLLVMType(node.retType);
            llvm::Value *zero = llvm::Constant::getNullValue(retTy);
            this->builder.CreateRet(zero);
        }
    }

    // clear symbols local to this function
    for (auto &param : node.params) {
        this->symbolTable.erase(param.name);
    }
}

void CodegenVisitor::visit(AstLet &node) {
    llvm::Type *llvmTy = toLLVMType(node.type);

    llvm::BasicBlock *currentBlock = this->builder.GetInsertBlock();
    if (!currentBlock) {
        THROW_CODEGEN(
            "IRBuilder has no insertion block when allocating variable",
            this->verbose);
    }
    llvm::Function *func = currentBlock->getParent();
    llvm::IRBuilder<> tmpBuilder(&func->getEntryBlock(),
                                 func->getEntryBlock().begin());
    llvm::AllocaInst *alloca =
        tmpBuilder.CreateAlloca(llvmTy, nullptr, node.name);

    if (node.expr) {
        llvm::Value *initVal = generateExpr(node.expr.get());

        this->builder.CreateStore(initVal, alloca);
    } else {
        // always store a value to prevent undefined behavior
        llvm::Value *zeroVal = llvm::Constant::getNullValue(llvmTy);
        this->builder.CreateStore(zeroVal, alloca);
    }

    this->declareSymbol(node.name, &node.type, node.mut, alloca);
}

void CodegenVisitor::visit(AstReturn &node) {
    llvm::Value *retVal = nullptr;
    if (node.expr) {
        retVal = generateExpr(node.expr.get());
    }

    if (retVal) {
        this->builder.CreateRet(retVal);
    } else {
        this->builder.CreateRetVoid();
    }
}

void CodegenVisitor::declareSymbol(const std::string &name, const Type *type,
                                   bool mut, llvm::Value *value) {
    this->symbolTable.insert_or_assign(
        name, VariableInfo(value, std::make_unique<Type>(*type), mut));
}

void CodegenVisitor::declareGlobals(const AstProgram &program) {
    for (const auto &decl : program.decls) {
        if (auto *fn = dynamic_cast<AstFn *>(decl.get())) {
            this->declareSymbol(fn->name, &fn->retType, /*mut=*/false,
                                this->generateFnPrototype(*fn));
        } else if (auto *let = dynamic_cast<AstLet *>(decl.get())) {
            this->generateGlobalVariable(*let);
        } else {
            THROW_CODEGEN("Only functions and variable declarations con be "
                          "declared at top level",
                          this->verbose);
        }
    }
}

llvm::Function *CodegenVisitor::generateFnPrototype(const AstFn &fn) {
    std::vector<llvm::Type *> paramTypes;
    for (const auto &param : fn.params) {
        paramTypes.push_back(toLLVMType(param.type));
    }

    llvm::FunctionType *fnType =
        llvm::FunctionType::get(toLLVMType(fn.retType), paramTypes, false);

    llvm::Function *function = llvm::Function::Create(
        fnType, llvm::Function::ExternalLinkage, fn.name, *this->module);

    unsigned idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(fn.params[idx++].name);
    }

    return function;
}

void CodegenVisitor::generateGlobalVariable(const AstLet &let) {
    llvm::Type *llvmTy = toLLVMType(let.type);

    llvm::Constant *initConstant = nullptr;

    if (let.expr) {
        llvm::Value *initVal = generateExpr(let.expr.get());

        if (auto *constVal = llvm::dyn_cast<llvm::Constant>(initVal)) {
            initConstant = constVal;
        } else {
            THROW_CODEGEN("Global variable initializer must be a constant",
                          this->verbose);
        }
    } else {
        initConstant = llvm::Constant::getNullValue(llvmTy);
    }

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *this->module, llvmTy,
        /*isConstant=*/!let.mut, llvm::GlobalValue::ExternalLinkage,
        initConstant, let.name);

    this->declareSymbol(let.name, &let.type, let.mut, globalVar);
}

llvm::Value *CodegenVisitor::generateExpr(AstNode *node) {
    if (!node) {
        return nullptr;
    }

    switch (node->kind()) {
    case AstNodeKind::Identifier:
        visit(static_cast<AstIdentifier &>(*node));
        break;
    case AstNodeKind::Literal:
        visit(static_cast<AstLiteral &>(*node));
        break;
    case AstNodeKind::Unary:
        visit(static_cast<AstUnary &>(*node));
        break;
    case AstNodeKind::Binary:
        visit(static_cast<AstBinary &>(*node));
        break;
    default:
        THROW_CODEGEN("Unsupported expression node", this->verbose);
    }

    return lastValue;
}

llvm::Type *CodegenVisitor::toLLVMType(const Type &type) {
    switch (type.kind) {
    case Type::Kind::Primary:
        switch (std::get<PrimaryType>(type.data)) {
        case PrimaryType::I8:
            return llvm::Type::getInt8Ty(*this->context);
        case PrimaryType::I16:
            return llvm::Type::getInt16Ty(*this->context);
        case PrimaryType::I32:
            return llvm::Type::getInt32Ty(*this->context);
        case PrimaryType::I64:
            return llvm::Type::getInt64Ty(*this->context);

        case PrimaryType::U8:
            return llvm::Type::getInt8Ty(*this->context);
        case PrimaryType::U16:
            return llvm::Type::getInt16Ty(*this->context);
        case PrimaryType::U32:
            return llvm::Type::getInt32Ty(*this->context);
        case PrimaryType::U64:
            return llvm::Type::getInt64Ty(*this->context);

        case PrimaryType::F32:
            return llvm::Type::getFloatTy(*this->context);
        case PrimaryType::F64:
            return llvm::Type::getDoubleTy(*this->context);

        case PrimaryType::Bool:
            return llvm::Type::getInt1Ty(*this->context);

        case PrimaryType::Void:
            return llvm::Type::getVoidTy(*this->context);

        case PrimaryType::String:
            return llvm::PointerType::getInt8Ty(*this->context);

        default:
            THROW_CODEGEN("Unknown primary type", this->verbose);
        }

    case Type::Kind::Array: {
        const auto &array = std::get<Type::ArrayData>(type.data);

        llvm::Type *elemTy = toLLVMType(*array.elementType);

        // attempt to extract constant integer from length expression
        auto *literalNode = dynamic_cast<AstLiteral *>(array.length.get());
        // TODO: identifier used as length
        if (!literalNode || !literalNode->value.isNumber()) {
            THROW_CODEGEN("Array length must be a numeric literal",
                          this->verbose);
        }

        const auto number = literalNode->value.getNumber();
        if (number.type != PrimaryType::I32 &&
            number.type != PrimaryType::U32) {
            THROW_CODEGEN("Array length must be i32 or u32", this->verbose);
        }

        uint64_t length = static_cast<uint64_t>(number.value);
        return llvm::ArrayType::get(elemTy, length);
    }

    default:
        THROW_CODEGEN("Unknown type kind", this->verbose);
    }
}

void CodegenVisitor::optimize() {
    llvm::PassBuilder passBuilder;

    llvm::FunctionAnalysisManager fam;
    llvm::LoopAnalysisManager lam;
    llvm::CGSCCAnalysisManager cgam;
    llvm::ModuleAnalysisManager mam;

    // register all the basic analyses with the managers.
    passBuilder.registerModuleAnalyses(mam);
    passBuilder.registerCGSCCAnalyses(cgam);
    passBuilder.registerFunctionAnalyses(fam);
    passBuilder.registerLoopAnalyses(lam);
    passBuilder.crossRegisterProxies(lam, fam, cgam, mam);

    llvm::OptimizationLevel optLevel;
    switch (this->optimization) {
    case Optimization::O0:
        return;
    case Optimization::O1:
        optLevel = llvm::OptimizationLevel::O1;
        break;
    case Optimization::O2:
        optLevel = llvm::OptimizationLevel::O2;
        break;
    case Optimization::O3:
        optLevel = llvm::OptimizationLevel::O3;
        break;
    case Optimization::Os:
        optLevel = llvm::OptimizationLevel::Os;
        break;
    case Optimization::Oz:
        optLevel = llvm::OptimizationLevel::Oz;
        break;
    default:
        THROW_CODEGEN("Unsupported optimization level", this->verbose);
    }
    llvm::FunctionPassManager fpm =
        passBuilder.buildFunctionSimplificationPipeline(
            optLevel, llvm::ThinOrFullLTOPhase::None);

    // run function-level optimizations
    for (llvm::Function &func : *this->module) {
        fpm.run(func, fam);
    }

    llvm::ModulePassManager mpm =
        passBuilder.buildPerModuleDefaultPipeline(optLevel);
    mpm.run(*this->module, mam);
}

void CodegenVisitor::emitObjectFile(const std::string &filename) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string targetTriple = llvm::sys::getDefaultTargetTriple();
    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
    if (!target) {
        THROW_CODEGEN(fmt::format("Target lookup failed: {}", error),
                      this->verbose);
    }

    std::string CPU = "generic";
    std::string features = "";

    llvm::TargetOptions opt;
    auto targetMachine = target->createTargetMachine(
        targetTriple, CPU, features, opt, llvm::Reloc::PIC_);
    this->module->setDataLayout(targetMachine->createDataLayout());
    this->module->setTargetTriple(targetTriple);

    std::error_code EC;
    llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        THROW_CODEGEN(fmt::format("Failed to open file: {}", EC.message()),
                      this->verbose);
    }

    llvm::legacy::PassManager pass;
    auto fileType = llvm::CodeGenFileType::ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
        THROW_CODEGEN(
            fmt::format("TargetMachine can't emit a file of `ObjectFile` type"),
            this->verbose);
    }

    pass.run(*this->module);
    dest.flush();
}
