#pragma once

#include <fmt/core.h>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <variant>

enum class PrimaryType {
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
    Bool,
    Void,
};

inline std::string primaryTypeToString(PrimaryType type) {
    switch (type) {
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
    case PrimaryType::Bool:
        return "bool";
    case PrimaryType::Void:
        return "void";
    default:
        return "unknown";
    }
}

// forward declaration
struct AstNode;
using AstNodePtr = std::unique_ptr<AstNode>;

struct Type {
    enum class Kind { Primary, Array } kind;

    struct ArrayData {
        std::unique_ptr<Type> elementType;
        AstNodePtr length;

        ArrayData(std::unique_ptr<Type> elem, AstNodePtr len)
            : elementType(std::move(elem)), length(std::move(len)) {}

        ArrayData(ArrayData &&other) noexcept = default;
        ArrayData &operator=(ArrayData &&other) noexcept = default;

        ~ArrayData() = default;
    };

    std::variant<PrimaryType, ArrayData> data;

    explicit Type(PrimaryType pt) : kind(Kind::Primary), data(pt) {}
    Type(std::unique_ptr<Type> elem, AstNodePtr len)
        : kind(Kind::Array), data(ArrayData(std::move(elem), std::move(len))) {}

    Type(Type &&) noexcept = default;
    Type &operator=(Type &&) noexcept = default;

    Type(const Type &other);

    ~Type() = default;

    std::string toString() const {
        std::ostringstream oss;
        if (kind == Kind::Primary) {
            return primaryTypeToString(std::get<PrimaryType>(data));
        } else if (kind == Kind::Array) {
            const auto &arr = std::get<ArrayData>(data);
            return fmt::format("arr<{}, _>", arr.elementType->toString());
        } else {
            return "UnknownType";
        }
    }

    bool isInteger() const {
        if (kind != Kind::Primary) {
            return false;
        }
        switch (std::get<PrimaryType>(data)) {
        case PrimaryType::I8:
        case PrimaryType::I16:
        case PrimaryType::I32:
        case PrimaryType::I64:
        case PrimaryType::U8:
        case PrimaryType::U16:
        case PrimaryType::U32:
        case PrimaryType::U64:
            return true;
        default:
            return false;
        }
    }

    bool isFloat() const {
        if (kind != Kind::Primary) {
            return false;
        }
        PrimaryType ty = std::get<PrimaryType>(data);
        return ty == PrimaryType::F32 || ty == PrimaryType::F64;
    }

    bool isSigned() const {
        if (kind != Kind::Primary) {
            return false;
        }
        switch (std::get<PrimaryType>(data)) {
        case PrimaryType::I8:
        case PrimaryType::I16:
        case PrimaryType::I32:
        case PrimaryType::I64:
            return true;
        default:
            return false;
        }
    }

    bool isUnsigned() const { return isInteger() && !isSigned(); }
};
