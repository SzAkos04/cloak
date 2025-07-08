#pragma once

#include "error.hpp"
#include "token.hpp"

#include <fmt/core.h>
#include <string>
#include <vector>

class LexerError : public Error {
    using Error::Error;
};

#define THROW_LEXER(line, msg, verbose)                                        \
    throw LexerError(fmt::format("Line {}: {}", (line), (msg)), (verbose),     \
                     __FILE__, __LINE__, __func__)

class Lexer {
  public:
    explicit Lexer(const std::string &src_, bool verbose_)
        : src(src_), start(0), cur(0), line(1), verbose(verbose_) {}

    std::vector<Token> lex();

  private:
    std::string src;
    size_t start;
    size_t cur;
    int line;

    bool verbose;

    bool isAtEnd() const;
    char peek() const;
    char peekNext() const;
    char advance();
    bool match(char exp);
    void skipWhitespace();
    Token makeToken(TokenType type);
    Token string();
    Token number();
    Token identifier();
};
