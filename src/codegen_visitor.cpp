#include "codegen_visitor.hpp"

#include "ast.hpp"

#include <fmt/core.h>
#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

void CodegenVisitor::visit(AstProgram &node) {
    this->declareFnPrototypes(node);

    for (const auto &decl : node.decls) {
        if (auto *fn = dynamic_cast<AstFn *>(decl.get())) {
            fn->accept(*this);
        } else {
            THROW_CODEGEN("Top-level non-function declarations not supported",
                          this->verbose);
        }
    }

    // verify the IR
    std::string errStr;
    llvm::raw_string_ostream errStream(errStr);

    if (llvm::verifyModule(*module, &errStream)) {
        THROW_CODEGEN(
            fmt::format("IR verification failed:\n{}", errStream.str()),
            verbose);
    }
}

void CodegenVisitor::visit(AstIdentifier &node) {
    auto it = namedValues.find(node.name);
    if (it == namedValues.end() || it->second == nullptr) {
        THROW_CODEGEN(
            fmt::format("Use of undefined identifier `{}`", node.name),
            verbose);
    }
    this->lastValue = it->second;
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
        case PrimaryType::U64:
            ty = toLLVMType(Type(number.type));
            constVal = llvm::ConstantInt::get(ty, number.value, false);
            break;

        case PrimaryType::F32:
            ty = llvm::Type::getFloatTy(*context);
            constVal = llvm::ConstantFP::get(ty, number.value);
            break;

        case PrimaryType::F64:
            ty = llvm::Type::getDoubleTy(*context);
            constVal = llvm::ConstantFP::get(ty, number.value);
            break;

        default:
            THROW_CODEGEN("Unsupported literal numeric type", verbose);
        }

        lastValue = constVal;
    } else if (val.isBool()) {
        lastValue = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context),
                                           val.getBool() ? 1 : 0);
    } else if (val.isString()) {
        const std::string &str = val.getString();
        llvm::Constant *strConstant =
            llvm::ConstantDataArray::getString(*context, str);
        llvm::GlobalVariable *strVar = new llvm::GlobalVariable(
            *module, strConstant->getType(), true,
            llvm::GlobalValue::PrivateLinkage, strConstant, ".str");

        lastValue = builder.CreatePointerCast(
            strVar, llvm::PointerType::getInt8Ty(*context));
    } else {
        THROW_CODEGEN("Unsupported literal type", verbose);
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

    switch (node.op) {
    case UnaryOp::Negate:
        if (operand->getType()->isFloatingPointTy()) {
            lastValue = builder.CreateFNeg(operand, "negtmp");
        } else if (operand->getType()->isIntegerTy()) {
            lastValue = builder.CreateNeg(operand, "negtmp");
        } else {
            THROW_CODEGEN("Invalid type for unary negate", verbose);
        }
        break;

    case UnaryOp::Not:
        lastValue = builder.CreateNot(operand, "nottmp");
        break;

    default:
        THROW_CODEGEN("Unsupported unary operator", verbose);
    }
}

void CodegenVisitor::visit(AstBinary &node) {
    node.lhs->accept(*this);
    llvm::Value *left = lastValue;

    node.rhs->accept(*this);
    llvm::Value *right = lastValue;

    bool isFloat = left->getType()->isFloatingPointTy();

    switch (node.op) {
    case BinaryOp::Add:
        lastValue = isFloat ? builder.CreateFAdd(left, right, "addtmp")
                            : builder.CreateAdd(left, right, "addtmp");
        break;

    case BinaryOp::Sub:
        lastValue = isFloat ? builder.CreateFSub(left, right, "subtmp")
                            : builder.CreateSub(left, right, "subtmp");
        break;

    case BinaryOp::Mul:
        lastValue = isFloat ? builder.CreateFMul(left, right, "multmp")
                            : builder.CreateMul(left, right, "multmp");
        break;

    case BinaryOp::Div:
        if (isFloat) {
            lastValue = builder.CreateFDiv(left, right, "divtmp");
        } else {
            lastValue = builder.CreateSDiv(
                left, right, "divtmp"); // TODO: Or CreateUDiv if unsigned
        }
        break;

    case BinaryOp::Eq:
        lastValue = isFloat ? builder.CreateFCmpUEQ(left, right, "eqtmp")
                            : builder.CreateICmpEQ(left, right, "eqtmp");
        break;

    case BinaryOp::Neq:
        lastValue = isFloat ? builder.CreateFCmpUNE(left, right, "neqtmp")
                            : builder.CreateICmpNE(left, right, "neqtmp");
        break;

    case BinaryOp::Lt:
        lastValue = isFloat ? builder.CreateFCmpULT(left, right, "lttmp")
                            : builder.CreateICmpSLT(left, right, "lttmp");
        break;

    case BinaryOp::Lte:
        lastValue = isFloat ? builder.CreateFCmpULE(left, right, "ltetmp")
                            : builder.CreateICmpSLE(left, right, "ltetmp");
        break;

    case BinaryOp::Gt:
        lastValue = isFloat ? builder.CreateFCmpUGT(left, right, "gttmp")
                            : builder.CreateICmpSGT(left, right, "gttmp");
        break;

    case BinaryOp::Gte:
        lastValue = isFloat ? builder.CreateFCmpUGE(left, right, "gtetmp")
                            : builder.CreateICmpSGE(left, right, "gtetmp");
        break;

    default:
        THROW_CODEGEN("Unsupported binary operator", verbose);
    }
}

void CodegenVisitor::visit(AstFn &node) {
    // retrieve the function prototype from the module
    llvm::Function *func = module->getFunction(node.name);
    if (!func) {
        THROW_CODEGEN(fmt::format("Function '{}' not declared", node.name),
                      verbose);
    }

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*context, "entry", func);
    builder.SetInsertPoint(entry);

    // register parameters in symbol table
    unsigned idx = 0;
    for (auto &arg : func->args()) {
        const std::string &pname = node.params[idx++].name;
        arg.setName(pname);
        namedValues[pname] = &arg;
    }

    node.body->accept(*this);

    // if no terminator, auto-generate
    if (!builder.GetInsertBlock()->getTerminator()) {
        if (node.retType.kind == Type::Kind::Primary &&
            node.retType.data.primary == PrimaryType::Void) {
            builder.CreateRetVoid();
        } else {
            llvm::Type *retTy = toLLVMType(node.retType);
            llvm::Value *zero = llvm::Constant::getNullValue(retTy);
            builder.CreateRet(zero);
        }
    }

    // clear symbols local to this function
    for (auto &param : node.params) {
        namedValues.erase(param.name);
    }
}

void CodegenVisitor::visit(AstReturn &node) {
    llvm::Value *retVal = nullptr;
    if (node.expr) {
        retVal = generateExpr(node.expr.get());
    }

    if (retVal) {
        builder.CreateRet(retVal);
    } else {
        builder.CreateRetVoid();
    }
}

void CodegenVisitor::declareFnPrototypes(const AstProgram &program) {
    for (const auto &decl : program.decls) {
        if (auto *fn = dynamic_cast<AstFn *>(decl.get())) {
            llvm::Function *function = generatePrototype(*fn);
            namedValues[fn->name] = function; // Optional: store for later use
        } else {
            THROW_CODEGEN("Only functions can be declared at top level",
                          verbose);
        }
    }
}

llvm::Function *CodegenVisitor::generatePrototype(const AstFn &fn) {
    std::vector<llvm::Type *> paramTypes;
    for (const auto &param : fn.params) {
        paramTypes.push_back(toLLVMType(param.type));
    }

    llvm::FunctionType *fnType =
        llvm::FunctionType::get(toLLVMType(fn.retType), paramTypes, false);

    llvm::Function *function = llvm::Function::Create(
        fnType, llvm::Function::ExternalLinkage, fn.name, *module);

    unsigned idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(fn.params[idx++].name);
    }

    return function;
}

llvm::Value *CodegenVisitor::generateExpr(AstNode *node) {
    if (!node) {
        return nullptr;
    }

    // Dispatch to the correct visit method based on node type:
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
    // Add other expression node kinds as needed.
    default:
        THROW_CODEGEN("Unsupported expression node", verbose);
    }

    return lastValue;
}

llvm::Type *CodegenVisitor::toLLVMType(const Type &type) {
    switch (type.kind) {
    case Type::Kind::Primary:
        switch (type.data.primary) {
        case PrimaryType::I8:
            return llvm::Type::getInt8Ty(*context);
        case PrimaryType::I16:
            return llvm::Type::getInt16Ty(*context);
        case PrimaryType::I32:
            return llvm::Type::getInt32Ty(*context);
        case PrimaryType::I64:
            return llvm::Type::getInt64Ty(*context);

        case PrimaryType::U8:
            return llvm::Type::getInt8Ty(*context);
        case PrimaryType::U16:
            return llvm::Type::getInt16Ty(*context);
        case PrimaryType::U32:
            return llvm::Type::getInt32Ty(*context);
        case PrimaryType::U64:
            return llvm::Type::getInt64Ty(*context);

        case PrimaryType::F32:
            return llvm::Type::getFloatTy(*context);
        case PrimaryType::F64:
            return llvm::Type::getDoubleTy(*context);

        case PrimaryType::Bool:
            return llvm::Type::getInt1Ty(*context);

        case PrimaryType::Void:
            return llvm::Type::getVoidTy(*context);

        case PrimaryType::String:
            return llvm::PointerType::getInt8Ty(*context);

        default:
            THROW_CODEGEN("Unknown primary type", this->verbose);
        }

    case Type::Kind::Array: {
        const auto &array = type.data.array;

        llvm::Type *elemTy = toLLVMType(*array.elementType);

        // attempt to extract constant integer from length expression
        auto *literalNode = dynamic_cast<AstLiteral *>(array.length.get());
        if (!literalNode || !literalNode->value.isNumber()) {
            THROW_CODEGEN("Array length must be a numeric literal", verbose);
        }

        const auto number = literalNode->value.getNumber();
        if (number.type != PrimaryType::I32 &&
            number.type != PrimaryType::U32) {
            THROW_CODEGEN("Array length must be i32 or u32", verbose);
        }

        uint64_t length = static_cast<uint64_t>(number.value);
        return llvm::ArrayType::get(elemTy, length);
    }

    default:
        THROW_CODEGEN("Unknown type kind", this->verbose);
    }
}

/* TODO:
void CodegenVisitor::emitObjectFile(const std::string &filename) {}
*/
