#pragma once

#include "ast.hpp"

struct AstPrintVisitor : AstVisitor {
    int indent_level = 0;

    void indent();

    void visit(AstProgram &node) override;
    void visit(AstIdentifier &node) override;
    void visit(AstLiteral &node) override;
    void visit(AstBlock &node) override;
    void visit(AstUnary &node) override;
    void visit(AstBinary &node) override;
    void visit(AstFn &node) override;
    void visit(AstReturn &node) override;
};
