#pragma once

#include "literal.hpp"
#include "token.hpp"
#include "type.hpp"

#include <memory>
#include <string>
#include <utility>
#include <vector>

struct AstVisitor;

enum class UnaryOp {
    Negate,
    Not,
};

inline bool isUnaryOp(TokenType type) {
    return type == TokenType::Minus || type == TokenType::Bang;
}

inline UnaryOp tokenTypeToUnaryOp(TokenType type) {
    switch (type) {
    case TokenType::Minus:
        return UnaryOp::Negate;
    case TokenType::Bang:
        return UnaryOp::Not;
    default:
        __builtin_unreachable();
    }
}

enum class BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
};

inline bool isBinaryOp(TokenType type) {
    return type == TokenType::Plus || type == TokenType::Minus ||
           type == TokenType::Star || type == TokenType::Slash ||
           type == TokenType::Percent || type == TokenType::EqualEqual ||
           type == TokenType::BangEqual || type == TokenType::Less ||
           type == TokenType::LessEqual || type == TokenType::Greater ||
           type == TokenType::GreaterEqual || type == TokenType::And ||
           type == TokenType::Or;
}

inline bool isLogicalBinaryOp(TokenType type) {
    return type == TokenType::EqualEqual || type == TokenType::BangEqual ||
           type == TokenType::Less || type == TokenType::LessEqual ||
           type == TokenType::Greater || type == TokenType::GreaterEqual ||
           type == TokenType::And || type == TokenType::Or;
}

inline BinaryOp tokenTypeToBinaryOp(TokenType type) {
    switch (type) {
    case TokenType::Plus:
        return BinaryOp::Add;
    case TokenType::Minus:
        return BinaryOp::Sub;
    case TokenType::Star:
        return BinaryOp::Mul;
    case TokenType::Slash:
        return BinaryOp::Div;
    case TokenType::Percent:
        return BinaryOp::Mod;
    case TokenType::EqualEqual:
        return BinaryOp::Eq;
    case TokenType::BangEqual:
        return BinaryOp::Neq;
    case TokenType::Less:
        return BinaryOp::Lt;
    case TokenType::LessEqual:
        return BinaryOp::Lte;
    case TokenType::Greater:
        return BinaryOp::Gt;
    case TokenType::GreaterEqual:
        return BinaryOp::Gte;
    case TokenType::And:
        return BinaryOp::And;
    case TokenType::Or:
        return BinaryOp::Or;
    default:
        __builtin_unreachable();
    }
}

enum class AssignmentOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
};

inline bool isAssignmentOp(TokenType type) {
    return type == TokenType::Equal || type == TokenType::PlusEqual ||
           type == TokenType::MinusEqual || type == TokenType::StarEqual ||
           type == TokenType::SlashEqual || type == TokenType::PercentEqual;
}

inline AssignmentOp tokenTypeToAssignmentOp(TokenType type) {
    switch (type) {
    case TokenType::Equal:
        return AssignmentOp::Assign;
    case TokenType::PlusEqual:
        return AssignmentOp::Add;
    case TokenType::MinusEqual:
        return AssignmentOp::Sub;
    case TokenType::StarEqual:
        return AssignmentOp::Mul;
    case TokenType::SlashEqual:
        return AssignmentOp::Div;
    case TokenType::PercentEqual:
        return AssignmentOp::Mod;
    default:
        __builtin_unreachable();
    }
}

struct AstNode;
using AstNodePtr = std::unique_ptr<AstNode>;

enum class AstNodeKind {
    Program,
    Identifier,
    Literal,
    Block,
    Unary,
    Binary,
    Assign,
    Fn,
    Let,
    Return,
};

//
// base AST node
//
struct AstNode {
    virtual ~AstNode() = default;
    virtual void accept(AstVisitor &visitor) = 0;
    virtual AstNodeKind kind() const;
    virtual AstNodePtr clone() const = 0;

    const Type *inferredType = nullptr;
};

// AST node subclasses

struct AstProgram : AstNode {
    std::vector<AstNodePtr> decls;

    AstProgram() = default;
    explicit AstProgram(std::vector<AstNodePtr> decls_)
        : decls(std::move(decls_)) {}

    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Program; }
    AstNodePtr clone() const override;
};

struct AstIdentifier : AstNode {
    std::string name;

    explicit AstIdentifier(std::string name_) : name(std::move(name_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Identifier; }
    AstNodePtr clone() const override;
};

struct AstLiteral : AstNode {
    Literal value;

    explicit AstLiteral(Literal value_) : value(std::move(value_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Literal; }
    AstNodePtr clone() const override;
};

struct AstBlock : AstNode {
    std::vector<AstNodePtr> stmts;

    AstBlock(std::vector<AstNodePtr> stmts_) : stmts(std::move(stmts_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Block; }
    AstNodePtr clone() const override;
};

struct AstUnary : AstNode {
    UnaryOp op;
    AstNodePtr rhs;

    AstUnary(UnaryOp op_, AstNodePtr rhs_) : op(op_), rhs(std::move(rhs_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Unary; }
    AstNodePtr clone() const override;
};

struct AstBinary : AstNode {
    AstNodePtr lhs;
    BinaryOp op;
    AstNodePtr rhs;

    AstBinary(AstNodePtr lhs_, BinaryOp op_, AstNodePtr rhs_)
        : lhs(std::move(lhs_)), op(op_), rhs(std::move(rhs_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Binary; }
    AstNodePtr clone() const override;
};

struct AstAssign : AstNode {
    std::string name;
    AssignmentOp op;
    AstNodePtr expr;

    AstAssign(std::string name_, AssignmentOp op_, AstNodePtr expr_)
        : name(std::move(name_)), op(op_), expr(std::move(expr_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Assign; }
    AstNodePtr clone() const override;
};

struct Param {
    std::string name;
    Type type;

    Param(std::string name_, Type type_)
        : name(std::move(name_)), type(std::move(type_)) {}

    Param(Param &&) noexcept = default;
    Param &operator=(Param &&) noexcept = default;
};

struct AstFn : AstNode {
    std::string name;
    std::vector<Param> params;
    Type retType;
    AstNodePtr body;

    AstFn(std::string name_, std::vector<Param> params_, Type retType_,
          AstNodePtr body_)
        : name(std::move(name_)), params(std::move(params_)),
          retType(std::move(retType_)), body(std::move(body_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Fn; }
    AstNodePtr clone() const override;
};

struct AstLet : AstNode {
    bool mut;
    std::string name;
    Type type;
    AstNodePtr expr;

    AstLet(bool mut_, std::string name_, Type type_, AstNodePtr expr_)
        : mut(mut_), name(std::move(name_)), type(std::move(type_)),
          expr(std::move(expr_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Let; }
    AstNodePtr clone() const override;
};

struct AstReturn : AstNode {
    AstNodePtr expr;

    AstReturn(AstNodePtr expr) : expr(std::move(expr)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Return; }
    AstNodePtr clone() const override;
};

//
// visitor interface
//

struct AstVisitor {
    virtual void visit(AstProgram &) {}
    virtual void visit(AstIdentifier &) {}
    virtual void visit(AstLiteral &) {}
    virtual void visit(AstBlock &) {}
    virtual void visit(AstUnary &) {}
    virtual void visit(AstBinary &) {}
    virtual void visit(AstAssign &) {}
    virtual void visit(AstFn &) {}
    virtual void visit(AstLet &) {}
    virtual void visit(AstReturn &) {}
    virtual ~AstVisitor() = default;
};

inline void AstProgram::accept(AstVisitor &v) { v.visit(*this); }
inline void AstIdentifier::accept(AstVisitor &v) { v.visit(*this); }
inline void AstLiteral::accept(AstVisitor &v) { v.visit(*this); }
inline void AstBlock::accept(AstVisitor &v) { v.visit(*this); }
inline void AstUnary::accept(AstVisitor &v) { v.visit(*this); }
inline void AstBinary::accept(AstVisitor &v) { v.visit(*this); }
inline void AstAssign::accept(AstVisitor &v) { v.visit(*this); }
inline void AstFn::accept(AstVisitor &v) { v.visit(*this); }
inline void AstLet::accept(AstVisitor &v) { v.visit(*this); }
inline void AstReturn::accept(AstVisitor &v) { v.visit(*this); }

inline AstNodeKind AstNode::kind() const { return AstNodeKind::Program; }

inline AstNodePtr AstProgram::clone() const {
    std::vector<AstNodePtr> clonedDecls;
    clonedDecls.reserve(decls.size());
    for (const auto &decl : decls) {
        clonedDecls.push_back(decl->clone());
    }
    return std::make_unique<AstProgram>(std::move(clonedDecls));
}

inline AstNodePtr AstIdentifier::clone() const {
    return std::make_unique<AstIdentifier>(name);
}

inline AstNodePtr AstLiteral::clone() const {
    return std::make_unique<AstLiteral>(value);
}

inline AstNodePtr AstBlock::clone() const {
    std::vector<AstNodePtr> clonedStmts;
    clonedStmts.reserve(stmts.size());
    for (const auto &stmt : stmts) {
        clonedStmts.push_back(stmt->clone());
    }
    return std::make_unique<AstBlock>(std::move(clonedStmts));
}

inline AstNodePtr AstUnary::clone() const {
    return std::make_unique<AstUnary>(op, rhs ? rhs->clone() : nullptr);
}

inline AstNodePtr AstBinary::clone() const {
    return std::make_unique<AstBinary>(lhs ? lhs->clone() : nullptr, op,
                                       rhs ? rhs->clone() : nullptr);
}

inline AstNodePtr AstAssign::clone() const {
    return std::make_unique<AstAssign>(name, op,
                                       expr ? expr->clone() : nullptr);
}

inline AstNodePtr AstFn::clone() const {
    std::vector<Param> clonedParams;
    clonedParams.reserve(params.size());
    for (const auto &param : params) {
        clonedParams.emplace_back(param.name, Type(param.type));
    }

    return std::make_unique<AstFn>(name, std::move(clonedParams), Type(retType),
                                   body ? body->clone() : nullptr);
}

inline AstNodePtr AstLet::clone() const {
    return std::make_unique<AstLet>(mut, name, Type(type),
                                    expr ? expr->clone() : nullptr);
}

inline AstNodePtr AstReturn::clone() const {
    return std::make_unique<AstReturn>(expr ? expr->clone() : nullptr);
}
