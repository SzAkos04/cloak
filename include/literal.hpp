#pragma once

#include <fmt/core.h>
#include <string>
#include <variant>

#include "type.hpp"

struct NumberLiteral {
    NumberLiteral(double value_, PrimaryType type_)
        : value(value_), type(type_) {}

    double value;
    PrimaryType type;
};

struct Literal {
    using Value = std::variant<std::monostate, NumberLiteral, bool>;

    Value value;

    explicit Literal(NumberLiteral n) : value(n) {}
    explicit Literal(bool b) : value(b) {}

    bool isNumber() const {
        return std::holds_alternative<NumberLiteral>(this->value);
    }
    bool isBool() const { return std::holds_alternative<bool>(this->value); }
    bool isVoid() const {
        return std::holds_alternative<std::monostate>(this->value);
    }

    PrimaryType getType() const {
        if (isNumber()) {
            return getNumber().type;
        }
        if (isBool()) {
            return PrimaryType::Bool;
        }
        return PrimaryType::Void;
    }

    NumberLiteral getNumber() const {
        return std::get<NumberLiteral>(this->value);
    }
    bool getBool() const { return std::get<bool>(this->value); }

    std::string toString() const {
        if (this->isNumber()) {
            return fmt::format("{} ({})", this->getNumber().value,
                               Type(this->getNumber().type).toString());
        } else if (this->isBool()) {
            return std::get<bool>(this->value) ? "true" : "false";
        } else if (this->isVoid()) {
            return "void";
        } else {
            return "unknown";
        }
    }
};
