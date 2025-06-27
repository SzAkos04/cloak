#include "ast.hpp"

// each accept calls visitor.visit with *this

void AstProgram::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstIdentifier::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstLiteral::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstBlock::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstUnary::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstBinary::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstFn::accept(AstVisitor &visitor) { visitor.visit(*this); }
void AstReturn::accept(AstVisitor &visitor) { visitor.visit(*this); }

// Type move and destructor definitions (simplified)

Type::Type(Type &&other) noexcept : kind(other.kind) {
    if (kind == Kind::Primary) {
        data.primary = other.data.primary;
    } else if (kind == Kind::Array) {
        new (&data.array) decltype(data.array)(std::move(other.data.array));
    }
}

Type &Type::operator=(Type &&other) noexcept {
    if (this != &other) {
        this->~Type();
        new (this) Type(std::move(other));
    }
    return *this;
}

Type::~Type() {
    if (kind == Kind::Array) {
        data.array.~ArrayData();
    }
}
