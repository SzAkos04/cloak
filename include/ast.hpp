#pragma once

#include "literal.hpp"
#include "token.hpp"
#include "type.hpp"

#include <memory>
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

struct AstNode;
using AstNodePtr = std::unique_ptr<AstNode>;

enum class AstNodeKind {
    Program,
    Identifier,
    Literal,
    Block,
    Unary,
    Binary,
    Fn,
    Return,
};

//
// base AST node
//
struct AstNode {
    virtual ~AstNode() = default;
    virtual void accept(AstVisitor &visitor) = 0;
    virtual AstNodeKind kind() const;

    PrimaryType inferredType = PrimaryType::Void;
};

// AST node subclasses

struct AstProgram : AstNode {
    std::vector<AstNodePtr> decls;

    AstProgram() = default;
    explicit AstProgram(std::vector<AstNodePtr> decls_)
        : decls(std::move(decls_)) {}

    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Program; }
};

struct AstIdentifier : AstNode {
    std::string name;

    explicit AstIdentifier(std::string name_) : name(std::move(name_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Identifier; }
};

struct AstLiteral : AstNode {
    Literal value;

    explicit AstLiteral(Literal value_) : value(std::move(value_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Literal; }
};

struct AstBlock : AstNode {
    std::vector<AstNodePtr> stmts;

    AstBlock(std::vector<AstNodePtr> stmts_) : stmts(std::move(stmts_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Block; }
};

struct AstUnary : AstNode {
    UnaryOp op;
    AstNodePtr rhs;

    AstUnary(UnaryOp op_, AstNodePtr rhs_) : op(op_), rhs(std::move(rhs_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Unary; }
};

struct AstBinary : AstNode {
    AstNodePtr lhs;
    BinaryOp op;
    AstNodePtr rhs;

    AstBinary(AstNodePtr lhs_, BinaryOp op_, AstNodePtr rhs_)
        : lhs(std::move(lhs_)), op(op_), rhs(std::move(rhs_)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Binary; }
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
};

struct AstReturn : AstNode {
    AstNodePtr expr;

    AstReturn(AstNodePtr expr) : expr(std::move(expr)) {}
    void accept(AstVisitor &visitor) override;
    AstNodeKind kind() const override { return AstNodeKind::Return; }
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
    virtual void visit(AstFn &) {}
    virtual void visit(AstReturn &) {}
    virtual ~AstVisitor() = default;
};

inline void AstProgram::accept(AstVisitor &v) { v.visit(*this); }
inline void AstIdentifier::accept(AstVisitor &v) { v.visit(*this); }
inline void AstLiteral::accept(AstVisitor &v) { v.visit(*this); }
inline void AstBlock::accept(AstVisitor &v) { v.visit(*this); }
inline void AstUnary::accept(AstVisitor &v) { v.visit(*this); }
inline void AstBinary::accept(AstVisitor &v) { v.visit(*this); }
inline void AstFn::accept(AstVisitor &v) { v.visit(*this); }
inline void AstReturn::accept(AstVisitor &v) { v.visit(*this); }

inline AstNodeKind AstNode::kind() const { return AstNodeKind::Program; }
