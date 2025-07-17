#include "printer_visitor.hpp"
#include "type.hpp"

#include <iostream>

void PrinterVisitor::indent() {
    for (int i = 0; i < this->indentLevel; ++i)
        std::cout << "  ";
}

void PrinterVisitor::visit(AstProgram &node) {
    std::cout << "Program:\n";
    this->indentLevel++;
    for (auto &decl : node.decls) {
        decl->accept(*this);
    }
    this->indentLevel--;
}

void PrinterVisitor::visit(AstIdentifier &node) {
    indent();
    std::cout << "Identifier: " << node.name << "\n";
}

void PrinterVisitor::visit(AstLiteral &node) {
    indent();
    std::cout << "Literal: ";
    if (node.value.isNumber()) {
        std::cout << node.value.toString();
    } else if (node.value.isString()) {
        std::cout << "\"" << std::get<std::string>(node.value.value)
                  << "\" (string)";
    } else if (node.value.isBool()) {
        std::cout << (std::get<bool>(node.value.value) ? "true" : "false")
                  << " (bool)";
    }
    std::cout << "\n";
}

void PrinterVisitor::visit(AstBlock &node) {
    indent();
    std::cout << "Block:\n";
    this->indentLevel++;
    for (auto &stmt : node.stmts) {
        stmt->accept(*this);
    }
    this->indentLevel--;
}

void PrinterVisitor::visit(AstUnary &node) {
    indent();
    std::cout << "UnaryOp: ";
    switch (node.op) {
    case UnaryOp::Negate:
        std::cout << "-\n";
        break;
    case UnaryOp::Not:
        std::cout << "!\n";
        break;
    }
    this->indentLevel++;
    node.rhs->accept(*this);
    this->indentLevel--;
}

void PrinterVisitor::visit(AstBinary &node) {
    indent();
    std::cout << "BinaryOp: ";
    switch (node.op) {
    case BinaryOp::Add:
        std::cout << "+";
        break;
    case BinaryOp::Sub:
        std::cout << "-";
        break;
    case BinaryOp::Mul:
        std::cout << "*";
        break;
    case BinaryOp::Div:
        std::cout << "/";
        break;
    case BinaryOp::Mod:
        std::cout << "%";
        break;
    case BinaryOp::Eq:
        std::cout << "==";
        break;
    case BinaryOp::Neq:
        std::cout << "!=";
        break;
    case BinaryOp::Lt:
        std::cout << "<";
        break;
    case BinaryOp::Lte:
        std::cout << "<=";
        break;
    case BinaryOp::Gt:
        std::cout << ">";
        break;
    case BinaryOp::Gte:
        std::cout << ">=";
        break;
    case BinaryOp::And:
        std::cout << "&&";
        break;
    case BinaryOp::Or:
        std::cout << "||";
        break;
    }
    std::cout << "\n";
    this->indentLevel++;
    node.lhs->accept(*this);
    node.rhs->accept(*this);
    this->indentLevel--;
}

void PrinterVisitor::visit(AstFn &node) {
    indent();
    std::cout << "Function: " << node.name << "(";
    for (size_t i = 0; i < node.params.size(); ++i) {
        const auto &param = node.params[i];
        std::cout << param.name << ": " << param.type.toString();
        if (i + 1 < node.params.size()) {
            std::cout << ", ";
        }
    }
    std::cout << ") -> " << node.retType.toString() << "\n";

    this->indentLevel++;
    node.body->accept(*this);
    this->indentLevel--;
}

void PrinterVisitor::visit(AstLet &node) {
    indent();
    std::cout << "Let: ";
    if (node.mut) {
        std::cout << "mut ";
    }
    std::cout << node.name << ": " << node.type.toString() << "\n";

    this->indentLevel++;
    if (node.expr) {
        indent();
        std::cout << "Initializer:\n";
        this->indentLevel++;
        node.expr->accept(*this);
        this->indentLevel--;
    } else {
        indent();
        std::cout << "(no initializer)\n";
    }
    this->indentLevel--;
}

void PrinterVisitor::visit(AstReturn &node) {
    indent();
    std::cout << "Return:\n";
    this->indentLevel++;
    if (node.expr) {
        node.expr->accept(*this);
    } else {
        indent();
        std::cout << "(void)\n";
    }
    this->indentLevel--;
}
