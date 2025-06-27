#pragma once

#include "token.hpp"

#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

class LexerError : public std::runtime_error {
  public:
    explicit LexerError(int line, const std::string &msg, bool verbose,
                        const char *file = nullptr, int line_ = 0,
                        const char *func = nullptr);

  private:
    static std::string formatMessage(int line, const std::string &msg,
                                     bool verbose, const char *file, int line_,
                                     const char *func);
};

#define THROW_LEXER(line, msg, verbose)                                        \
    throw LexerError((line), (msg), (verbose), __FILE__, __LINE__, __func__)

class Lexer {
  public:
    explicit Lexer(const std::string &src, bool verb);
    std::vector<Token> lex();

  private:
    std::string src;
    size_t start;
    size_t cur;
    int line;

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
