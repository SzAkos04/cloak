#include "type.hpp"

#include "ast.hpp"

#include <memory>

Type::Type(const Type &other) : kind(other.kind) {
    if (kind == Kind::Primary) {
        data = std::get<PrimaryType>(other.data);
    } else if (kind == Kind::Array) {
        const auto &arr = std::get<ArrayData>(other.data);
        auto elementCopy = std::make_unique<Type>(*arr.elementType);
        auto lengthCopy = arr.length ? arr.length->clone() : nullptr;
        data = ArrayData(std::move(elementCopy), std::move(lengthCopy));
    }
}
