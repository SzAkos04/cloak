#include "token.hpp"
#include <string>

Token::Token(TokenType type, const std::string &lexeme, int len, int line)
    : type(type), lexeme(lexeme), len(len), line(line) {}

// Optional: for debugging / printing
std::string tokenTypeToString(TokenType type) {
    switch (type) {
    case TokenType::LPAREN:
        return "LPAREN";
    case TokenType::RPAREN:
        return "RPAREN";
    case TokenType::LBRACE:
        return "LBRACE";
    case TokenType::RBRACE:
        return "RBRACE";
    case TokenType::LBRACKET:
        return "LBRACKET";
    case TokenType::RBRACKET:
        return "RBRACKET";
    case TokenType::COLON:
        return "COLON";
    case TokenType::SEMICOLON:
        return "SEMICOLON";
    case TokenType::COMMA:
        return "COMMA";

    case TokenType::EQUAL:
        return "EQUAL";
    case TokenType::EQUALEQUAL:
        return "EQUALEQUAL";
    case TokenType::BANG:
        return "BANG";
    case TokenType::BANGEQUAL:
        return "BANGEQUAL";
    case TokenType::PLUS:
        return "PLUS";
    case TokenType::PLUSEQUAL:
        return "PLUSEQUAL";
    case TokenType::MINUS:
        return "MINUS";
    case TokenType::MINUSEQUAL:
        return "MINUSEQUAL";
    case TokenType::STAR:
        return "STAR";
    case TokenType::STAREQUAL:
        return "STAREQUAL";
    case TokenType::SLASH:
        return "SLASH";
    case TokenType::SLASHEQUAL:
        return "SLASHEQUAL";
    case TokenType::PERCENT:
        return "PERCENT";
    case TokenType::PERCENTEQUAL:
        return "PERCENTEQUAL";
    case TokenType::LESS:
        return "LESS";
    case TokenType::LESSEQUAL:
        return "LESSEQUAL";
    case TokenType::GREATER:
        return "GREATER";
    case TokenType::GREATEREQUAL:
        return "GREATEREQUAL";
    case TokenType::AND:
        return "AND";
    case TokenType::OR:
        return "OR";

    case TokenType::IDENTIFIER:
        return "IDENTIFIER";
    case TokenType::FN:
        return "FN";
    case TokenType::NUMBER:
        return "NUMBER";
    case TokenType::STRING:
        return "STRING";
    case TokenType::BOOL:
        return "BOOL";
    case TokenType::EOF_:
        return "EOF";
    default:
        return "UNKNOWN";
    }
}

std::ostream &operator<<(std::ostream &os, const Token &tok) {
    os << std::string("[") << tokenTypeToString(tok.type) << std::string("] ")
       << std::string("\"") << tok.lexeme << std::string("\" (len=")
       << std::to_string(tok.len) << std::string(", line=")
       << std::to_string(tok.line) << std::string(")");
    return os;
}
