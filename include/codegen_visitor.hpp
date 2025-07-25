#pragma once

#include "ast.hpp"
#include "cli.hpp"
#include "error.hpp"
#include "type.hpp"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

#include <memory>
#include <string>
#include <unordered_map>

class CodegenError : public Error {
    using Error::Error;
};

#define THROW_CODEGEN(msg, verbose)                                            \
    throw CodegenError((msg), (verbose), __FILE__, __LINE__, __func__)

class CodegenVisitor : public AstVisitor {
  public:
    explicit CodegenVisitor(const std::string &filename_,
                            Optimization optimization_, bool verbose_)
        : context(std::make_unique<llvm::LLVMContext>()),
          module(std::make_unique<llvm::Module>("main_module", *context)),
          builder(*context), filename(filename_), optimization(optimization_),
          verbose(verbose_) {}

    void visit(AstProgram &node) override;
    void visit(AstIdentifier &node) override;
    void visit(AstLiteral &node) override;
    void visit(AstBlock &node) override;
    void visit(AstUnary &node) override;
    void visit(AstBinary &node) override;
    void visit(AstAssign &node) override;
    void visit(AstFn &node) override;
    void visit(AstLet &node) override;
    void visit(AstReturn &node) override;

    llvm::Module *getModule() const { return module.get(); }
    llvm::LLVMContext *getContext() const { return context.get(); }

    void dumpIR() const { this->module->print(llvm::outs(), nullptr); }

  private:
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    struct VariableInfo {
        llvm::Value *value;
        std::unique_ptr<Type> type;
        bool mut;

        VariableInfo(llvm::Value *value_, std::unique_ptr<Type> type_,
                     bool mut_)
            : value(std::move(value_)), type(std::move(type_)), mut(mut_) {}
    };

    std::unordered_map<std::string, VariableInfo> symbolTable;
    llvm::Value *lastValue = nullptr;

    std::string filename;
    Optimization optimization;
    bool verbose;

    // Helper methods
    void declareSymbol(const std::string &name, const Type *type, bool mut,
                       llvm::Value *value = nullptr);
    void declareGlobals(const AstProgram &program);
    llvm::Function *generateFnPrototype(const AstFn &fn);
    void generateGlobalVariable(const AstLet &let);
    llvm::Value *generateExpr(AstNode *node);
    llvm::Type *toLLVMType(const Type &type);

    void optimize();

    void emitObjectFile(const std::string &filename);
};
