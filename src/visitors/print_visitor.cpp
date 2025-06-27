#include "visitors/print_visitor.hpp"

#include <iostream>

void AstPrintVisitor::indent() {
    for (int i = 0; i < indent_level; ++i)
        std::cout << "  ";
}

void AstPrintVisitor::visit(AstProgram &node) {
    std::cout << "Program:\n";
    indent_level++;
    for (auto &decl : node.decls) {
        decl->accept(*this);
    }
    indent_level--;
}

void AstPrintVisitor::visit(AstIdentifier &node) {
    indent();
    std::cout << "Identifier: " << node.name << "\n";
}

void AstPrintVisitor::visit(AstLiteral &node) {
    indent();
    std::cout << "Literal: ";
    if (node.value.isNumber()) {
        std::cout << node.value.getNumber() << " ("
                  << primary_type_to_str(node.value.getNumType()) << ")";
    } else if (node.value.isString()) {
        std::cout << '"' << std::get<std::string>(node.value.value) << '"';
    } else if (node.value.isBool()) {
        std::cout << (std::get<bool>(node.value.value) ? "true" : "false");
    }
    std::cout << "\n";
}

void AstPrintVisitor::visit(AstBlock &node) {
    indent();
    std::cout << "Block:\n";
    indent_level++;
    for (auto &stmt : node.stmts) {
        stmt->accept(*this);
    }
    indent_level--;
}

void AstPrintVisitor::visit(AstUnary &node) {
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
    indent_level++;
    node.right->accept(*this);
    indent_level--;
}

void AstPrintVisitor::visit(AstBinary &node) {
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
    indent_level++;
    node.left->accept(*this);
    node.right->accept(*this);
    indent_level--;
}

void AstPrintVisitor::visit(AstFn &node) {
    indent();
    std::cout << "Function: " << node.name << "(";
    for (size_t i = 0; i < node.params.size(); ++i) {
        const auto &param = node.params[i];
        std::cout << param.name << ": "
                  << primary_type_to_str(param.type.data.primary);
        if (i + 1 < node.params.size())
            std::cout << ", ";
    }
    std::cout << ") -> " << primary_type_to_str(node.ret_type.data.primary)
              << "\n";

    indent_level++;
    node.body->accept(*this);
    indent_level--;
}
