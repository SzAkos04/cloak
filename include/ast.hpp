#pragma once

#include "token.hpp"

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

// forward declaration
struct AstVisitor;

enum class PrimaryType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Void
};

inline std::string primary_type_to_str(PrimaryType kind) {
    switch (kind) {
    case PrimaryType::Bool:
        return "bool";
    case PrimaryType::I8:
        return "i8";
    case PrimaryType::I16:
        return "i16";
    case PrimaryType::I32:
        return "i32";
    case PrimaryType::I64:
        return "i64";
    case PrimaryType::U8:
        return "u8";
    case PrimaryType::U16:
        return "u16";
    case PrimaryType::U32:
        return "u32";
    case PrimaryType::U64:
        return "u64";
    case PrimaryType::F32:
        return "f32";
    case PrimaryType::F64:
        return "f64";
    case PrimaryType::String:
        return "string";
    case PrimaryType::Void:
        return "void";
    default:
        return "unknown";
    }
}

enum class UnaryOp {
    Negate,
    Not,
};

inline bool isUnaryOp(TokenType type) {
    return type == TokenType::MINUS || type == TokenType::BANG;
}

inline UnaryOp tokenTypeToUnaryOp(TokenType type) {
    switch (type) {
    case TokenType::MINUS:
        return UnaryOp::Negate;
    case TokenType::BANG:
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
    Or
};

inline bool isBinaryOp(TokenType type) {
    return type == TokenType::PLUS || type == TokenType::MINUS ||
           type == TokenType::STAR || type == TokenType::SLASH ||
           type == TokenType::PERCENT || type == TokenType::EQUALEQUAL ||
           type == TokenType::BANGEQUAL || type == TokenType::LESS ||
           type == TokenType::LESSEQUAL || type == TokenType::GREATER ||
           type == TokenType::GREATEREQUAL || type == TokenType::AND ||
           type == TokenType::OR;
}

inline bool isLogicalBinaryOp(TokenType type) {
    return type == TokenType::EQUALEQUAL || type == TokenType::BANGEQUAL ||
           type == TokenType::LESS || type == TokenType::LESSEQUAL ||
           type == TokenType::GREATER || type == TokenType::GREATEREQUAL ||
           type == TokenType::AND || type == TokenType::OR;
}

inline BinaryOp tokenTypeToBinaryOp(TokenType type) {
    switch (type) {
    case TokenType::PLUS:
        return BinaryOp::Add;
    case TokenType::MINUS:
        return BinaryOp::Sub;
    case TokenType::STAR:
        return BinaryOp::Mul;
    case TokenType::SLASH:
        return BinaryOp::Div;
    case TokenType::PERCENT:
        return BinaryOp::Mod;
    case TokenType::EQUALEQUAL:
        return BinaryOp::Eq;
    case TokenType::BANGEQUAL:
        return BinaryOp::Neq;
    case TokenType::LESS:
        return BinaryOp::Lt;
    case TokenType::LESSEQUAL:
        return BinaryOp::Lte;
    case TokenType::GREATER:
        return BinaryOp::Gt;
    case TokenType::GREATEREQUAL:
        return BinaryOp::Gte;
    case TokenType::AND:
        return BinaryOp::And;
    case TokenType::OR:
        return BinaryOp::Or;
    default:
        __builtin_unreachable();
    }
}

// forward declaration
struct AstNode;
using AstNodePtr = std::unique_ptr<AstNode>;

struct Type {
    enum Kind { Primary, Array } kind;

    union Data {
        PrimaryType primary;
        struct ArrayData {
            std::unique_ptr<Type> element_type;
            std::unique_ptr<AstNode> length;
            ArrayData() = default;
            ArrayData(ArrayData &&) = default;
            ArrayData &operator=(ArrayData &&) = default;
            ~ArrayData() = default;
        } array;

        Data() {}
        ~Data() {}
    } data;

    explicit Type(PrimaryType pt) : kind(Kind::Primary) { data.primary = pt; }
    Type(std::unique_ptr<Type> elem, std::unique_ptr<AstNode> len)
        : kind(Array) {
        new (&data.array) Data::ArrayData{std::move(elem), std::move(len)};
    }
    Type(Type &&) noexcept;
    Type &operator=(Type &&) noexcept;
    ~Type();
};

struct NumberLiteral {
    double number;
    PrimaryType num_type;
    NumberLiteral(double num, PrimaryType type) : number(num), num_type(type) {}
};

struct Literal {
    using Value = std::variant<NumberLiteral, std::string, bool>;

    Value value;

    Literal(double n, PrimaryType pt) : value(NumberLiteral{n, pt}) {}
    Literal(std::string s) : value(std::move(s)) {}
    Literal(bool b) : value(b) {}

    bool isNumber() const {
        return std::holds_alternative<NumberLiteral>(value);
    }
    bool isString() const { return std::holds_alternative<std::string>(value); }
    bool isBool() const { return std::holds_alternative<bool>(value); }

    double getNumber() const { return std::get<NumberLiteral>(value).number; }
    PrimaryType getNumType() const {
        return std::get<NumberLiteral>(value).num_type;
    }
};

//
// base AST node
//
struct AstNode {
    virtual ~AstNode() = default;
    virtual void accept(AstVisitor &visitor) = 0;
};

// AST node subclasses

struct AstProgram : AstNode {
    std::vector<AstNodePtr> decls;

    AstProgram() = default;
    explicit AstProgram(std::vector<AstNodePtr> decls_)
        : decls(std::move(decls_)) {}

    void accept(AstVisitor &visitor) override;
};

struct AstIdentifier : AstNode {
    std::string name;

    explicit AstIdentifier(std::string n) : name(std::move(n)) {}
    void accept(AstVisitor &visitor) override;
};

struct AstLiteral : AstNode {
    Literal value;

    explicit AstLiteral(Literal val) : value(std::move(val)) {}
    void accept(AstVisitor &visitor) override;
};

struct AstBlock : AstNode {
    std::vector<AstNodePtr> stmts;

    AstBlock(std::vector<AstNodePtr> stmts_) : stmts(std::move(stmts_)) {}
    void accept(AstVisitor &visitor) override;
};

struct AstUnary : AstNode {
    UnaryOp op;
    AstNodePtr right;

    AstUnary(UnaryOp o, AstNodePtr r) : op(o), right(std::move(r)) {}
    void accept(AstVisitor &visitor) override;
};

struct AstBinary : AstNode {
    AstNodePtr left;
    BinaryOp op;
    AstNodePtr right;

    AstBinary(AstNodePtr l, BinaryOp o, AstNodePtr r)
        : left(std::move(l)), op(o), right(std::move(r)) {}
    void accept(AstVisitor &visitor) override;
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
    Type ret_type;
    AstNodePtr body;

    AstFn(std::string n, std::vector<Param> p, Type rt, AstNodePtr b)
        : name(std::move(n)), params(std::move(p)), ret_type(std::move(rt)),
          body(std::move(b)) {}
    void accept(AstVisitor &visitor) override;
};

//
// visitor interface
//

struct AstVisitor {
    virtual ~AstVisitor() = default;

    virtual void visit(AstProgram &node) = 0;

    virtual void visit(AstIdentifier &node) = 0;
    virtual void visit(AstLiteral &node) = 0;
    virtual void visit(AstBlock &node) = 0;
    virtual void visit(AstUnary &node) = 0;
    virtual void visit(AstBinary &node) = 0;

    virtual void visit(AstFn &node) = 0;
};
