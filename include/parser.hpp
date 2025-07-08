#pragma once

#include "ast.hpp"
#include "error.hpp"
#include "token.hpp"
#include "type.hpp"

#include <fmt/core.h>
#include <vector>

class ParserError : public Error {
    using Error::Error;
};

#define THROW_PARSER(line, msg, verbose)                                       \
    throw ParserError(fmt::format("Line {}: {}", (line), (msg)), (verbose),    \
                      __FILE__, __LINE__, __func__)

class Parser {
  public:
    explicit Parser(const std::vector<Token> &tokens_, bool verbose_)
        : tokens(tokens_), current(0), verbose(verbose_) {}

    std::unique_ptr<AstProgram> parseProgram();

  private:
    const std::vector<Token> &tokens;
    size_t current;

    bool verbose;

    bool isAtEnd() const;
    const Token &peek() const;
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
    AstNodePtr parseReturn();
};
