#pragma once

#include "ast.hpp"
#include "token.hpp"
#include <cstddef>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

class ParserError : public std::runtime_error {
  public:
    explicit ParserError(int line, const std::string &msg, bool verbose,
                         const char *file = nullptr, int line_ = 0,
                         const char *func = nullptr);

  private:
    static std::string formatMessage(int line, const std::string &msg,
                                     bool verbose, const char *file, int line_,
                                     const char *func);
};

#define THROW_PARSER(line, msg, verbose)                                       \
    throw ParserError((line), (msg), (verbose), __FILE__, __LINE__, __func__)

class Parser {
  public:
    explicit Parser(const std::vector<Token> &tokens, bool verb);
    std::unique_ptr<AstProgram> parseProgram();

  private:
    const std::vector<Token> &tokens;
    size_t current;

    bool verbose;

    bool isAtEnd() const;
    const Token &peek() const;
    const Token &previous() const;
    const Token &advance();
    bool check(TokenType type) const;
    bool match(TokenType type);

    AstNodePtr parseDecl();

    AstNodePtr parseStmt();

    Type parseType();
    // utility for parsing arrays
    Type parseArr();

    AstNodePtr parseExpr();
    AstNodePtr parsePrimary();
    AstNodePtr parseUnaryExpr();
    AstNodePtr parseBinaryRhs(int precedence, std::unique_ptr<AstNode> lhs);
    // utility for operator precedence
    int getPrecedence(TokenType type) const;

    AstNodePtr parseBlock();

    AstNodePtr parseFn();
};
