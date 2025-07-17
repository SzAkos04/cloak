#pragma once

#include "ast.hpp"

class PrinterVisitor : public AstVisitor {
  public:
    void visit(AstProgram &node) override;
    void visit(AstIdentifier &node) override;
    void visit(AstLiteral &node) override;
    void visit(AstBlock &node) override;
    void visit(AstUnary &node) override;
    void visit(AstBinary &node) override;
    void visit(AstFn &node) override;
    void visit(AstLet &node) override;
    void visit(AstReturn &node) override;

  private:
    int indentLevel = 0;
    void indent();
};
