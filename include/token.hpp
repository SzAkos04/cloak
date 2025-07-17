#pragma once

#include <fmt/core.h>
#include <string>

enum class TokenType {
    // one char tokens
    LParen,    // `(`
    RParen,    // `)`
    LBrace,    // `{`
    RBrace,    // `}`
    LBracket,  // `[`
    RBracket,  // `]`
    Colon,     // `:`
    Semicolon, // `;`
    Comma,     // `,`

    // one or two char tokens
    Equal,        // `=`
    EqualEqual,   // `==`
    Bang,         // `!`
    BangEqual,    // `!=`
    Plus,         // `+`
    PlusEqual,    // `+=`
    Minus,        // `-`
    MinusEqual,   // `-=`
    Star,         // `*`
    StarEqual,    // `*=`
    Slash,        // `/`
    SlashEqual,   // `/=`
    Percent,      // `%`
    PercentEqual, // `%=`
    Less,         // `<`
    LessEqual,    // `<=`
    Greater,      // `>`
    GreaterEqual, // >=

    And, // `&&`
    Or,  // `||`

    Identifier,

    Fn,     // `fn`
    Let,    // `let`
    Mut,    // `mut`
    Return, // `return`

    Number,
    String,
    Bool,

    Eof,
};

class Token {
  public:
    Token(TokenType type_, const std::string &lexeme_, int len_, int line_)
        : type(type_), lexeme(lexeme_), len(len_), line(line_) {}

    TokenType getType() const { return this->type; }
    const std::string &getLexeme() const { return this->lexeme; }
    int getLine() const { return this->line; }
    int getLength() const { return this->len; }

    std::string toString() const {
        return fmt::format("Token {{\n"
                           "  type   = {},\n"
                           "  lexeme = `{}`,\n"
                           "  len    = {},\n"
                           "  line   = {}\n"
                           "}}",
                           tokenTypeToString(this->type), this->lexeme,
                           this->len, this->line);
    }

  private:
    TokenType type;
    std::string lexeme;
    int len;
    int line;

    static std::string tokenTypeToString(TokenType type) {
        switch (type) {
        case TokenType::LParen:
            return "LParen";
        case TokenType::RParen:
            return "RParen";
        case TokenType::LBrace:
            return "LBrace";
        case TokenType::RBrace:
            return "RBrace";
        case TokenType::LBracket:
            return "LBracket";
        case TokenType::RBracket:
            return "RBracket";
        case TokenType::Colon:
            return "Colon";
        case TokenType::Semicolon:
            return "Semicolon";
        case TokenType::Comma:
            return "Comma";

        case TokenType::Equal:
            return "Equal";
        case TokenType::EqualEqual:
            return "EqualEqual";
        case TokenType::Bang:
            return "Bang";
        case TokenType::BangEqual:
            return "BangEqual";
        case TokenType::Plus:
            return "Plus";
        case TokenType::PlusEqual:
            return "PlusEqual";
        case TokenType::Minus:
            return "Minus";
        case TokenType::MinusEqual:
            return "MinusEqual";
        case TokenType::Star:
            return "Star";
        case TokenType::StarEqual:
            return "StarEqual";
        case TokenType::Slash:
            return "Slash";
        case TokenType::SlashEqual:
            return "SlashEqual";
        case TokenType::Percent:
            return "Percent";
        case TokenType::PercentEqual:
            return "PercentEqual";
        case TokenType::Less:
            return "Less";
        case TokenType::LessEqual:
            return "LessEqual";
        case TokenType::Greater:
            return "Greater";
        case TokenType::GreaterEqual:
            return "GreaterEqual";

        case TokenType::And:
            return "And";
        case TokenType::Or:
            return "Or";

        case TokenType::Identifier:
            return "Identifier";
        case TokenType::Fn:
            return "Fn";

        case TokenType::Let:
            return "Let";
        case TokenType::Mut:
            return "Mut";
        case TokenType::Return:
            return "Return";

        case TokenType::Number:
            return "Number";
        case TokenType::String:
            return "String";
        case TokenType::Bool:
            return "Bool";
        case TokenType::Eof:
            return "Eof";

        default:
            return "Unknown";
        }
    }
};
