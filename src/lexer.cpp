#include "lexer.hpp"
#include "token.hpp"

#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

LexerError::LexerError(int line, const std::string &msg, bool verbose,
                       const char *file, int line_, const char *func)
    : std::runtime_error(
          this->formatMessage(line, msg, verbose, file, line_, func)) {}

std::string LexerError::formatMessage(int line, const std::string &msg,
                                      bool verbose, const char *file, int line_,
                                      const char *func) {
    std::ostringstream oss;
    if (!verbose || file == nullptr || func == nullptr || line == 0) {
        oss << "Line " << std::to_string(line_) << ": " << msg;
    } else {
        oss << file << ":" << line << " (" << func << "): " << "Line "
            << std::to_string(line_) << ": " << msg;
    }
    return oss.str();
}

bool verbose;

Lexer::Lexer(const std::string &src, bool verb)
    : src(src), start(0), cur(0), line(1) {
    verbose = verb;
}

std::vector<Token> Lexer::lex() {
    std::vector<Token> tokens;

    while (!this->isAtEnd()) {
        this->skipWhitespace();
        this->start = this->cur;

        if (this->isAtEnd()) {
            break;
        }

        char c = this->advance();

        switch (c) {
        case '(':
            tokens.push_back(this->makeToken(TokenType::LPAREN));
            break;
        case ')':
            tokens.push_back(this->makeToken(TokenType::RPAREN));
            break;
        case '{':
            tokens.push_back(this->makeToken(TokenType::LBRACE));
            break;
        case '}':
            tokens.push_back(this->makeToken(TokenType::RBRACE));
            break;
        case '[':
            tokens.push_back(this->makeToken(TokenType::LBRACKET));
            break;
        case ']':
            tokens.push_back(this->makeToken(TokenType::RBRACKET));
            break;
        case ':':
            tokens.push_back(this->makeToken(TokenType::COLON));
            break;
        case ';':
            tokens.push_back(this->makeToken(TokenType::SEMICOLON));
            break;
        case ',':
            tokens.push_back(this->makeToken(TokenType::COMMA));
            break;

        case '=':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::EQUALEQUAL : TokenType::EQUAL));
            break;
        case '!':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::BANGEQUAL : TokenType::BANG));
            break;
        case '+':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::PLUSEQUAL : TokenType::PLUS));
            break;
        case '-':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::MINUSEQUAL : TokenType::MINUS));
            break;
        case '*':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::STAREQUAL : TokenType::STAR));
            break;
        case '/':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::SLASHEQUAL : TokenType::SLASH));
            break;
        case '%':
            tokens.push_back(this->makeToken(this->match('=')
                                                 ? TokenType::PERCENTEQUAL
                                                 : TokenType::PERCENT));
            break;
        case '<':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::LESSEQUAL : TokenType::LESS));
            break;
        case '>':
            tokens.push_back(this->makeToken(this->match('=')
                                                 ? TokenType::GREATEREQUAL
                                                 : TokenType::GREATER));
            break;

        case '&':
            if (this->match('&')) {
                tokens.push_back(this->makeToken(TokenType::AND));
            } else {
                THROW_LEXER(this->line, "Unexpected character '&'", verbose);
            }
            break;
        case '|':
            if (this->match('|')) {
                tokens.push_back(this->makeToken(TokenType::OR));
            } else {
                THROW_LEXER(this->line, "Unexpected character '|'", verbose);
            }
            break;
        case '"':
            tokens.push_back(this->string());
            break;

        default:
            if (std::isdigit(c)) {
                this->cur--; // rewind so `number()` sees the digit
                tokens.push_back(this->number());
            } else if (std::isalpha(c) || c == '_') {
                this->cur--; // rewind for identifier
                tokens.push_back(this->identifier());
            } else {
                THROW_LEXER(this->line,
                            std::string("Unexpected character '") + c + "'",
                            verbose);
            }
            break;
        }
    }

    tokens.push_back(Token(TokenType::EOF_, "", 0, this->line));
    return tokens;
}

bool Lexer::isAtEnd() const { return this->cur >= this->src.length(); }

char Lexer::peek() const {
    return this->isAtEnd() ? '\0' : this->src[this->cur];
}

char Lexer::peekNext() const {
    return (this->cur + 1 < this->src.size()) ? this->src[this->cur + 1] : '\0';
}

char Lexer::advance() { return this->src[this->cur++]; }

bool Lexer::match(char exp) {
    if (this->isAtEnd() || this->src[this->cur] != exp) {
        return false;
    }
    this->cur++;
    return true;
}

void Lexer::skipWhitespace() {
    while (!this->isAtEnd()) {
        char c = this->peek();
        switch (c) {
        case ' ':
        case '\r':
            this->advance();
            break;
        case '\n':
            this->line++;
            this->advance();
            break;
        case '/':
            if (this->peekNext() == '/') {
                while (!this->isAtEnd() && this->peek() != '\n') {
                    this->advance();
                }
            } else {
                return;
            }
            break;
        default:
            return;
        }
    }
}

Token Lexer::makeToken(TokenType type) {
    std::string lexeme = this->src.substr(this->start, this->cur - this->start);
    return Token(type, lexeme, (int)lexeme.length(), this->line);
}

Token Lexer::string() {
    while (!this->isAtEnd() && this->peek() != '"') {
        if (this->peek() == '\n') {
            this->line++;
        }
        this->advance();
    }

    if (this->isAtEnd()) {
        THROW_LEXER(this->line, "Unterminated string.", verbose);
    }

    this->advance(); // consume `"`
    std::string content =
        this->src.substr(this->start + 1, this->cur - this->start - 2);
    return Token(TokenType::STRING, content, (int)content.length(), this->line);
}

Token Lexer::number() {
    while (std::isdigit(this->peek())) {
        this->advance();
    }

    if (this->peek() == '.' && std::isdigit(this->peekNext())) {
        this->advance();
        while (std::isdigit(this->peek())) {
            this->advance();
        }
    }

    return this->makeToken(TokenType::NUMBER);
}

Token Lexer::identifier() {
    while (std::isalnum(this->peek()) || this->peek() == '_') {
        this->advance();
    }

    std::string text = this->src.substr(this->start, this->cur - this->start);

    if (text == "true" || text == "false") {
        return Token(TokenType::BOOL, text, (int)text.length(), this->line);
    }

    static const std::unordered_map<std::string, TokenType> keywords = {
        {"fn", TokenType::FN},
    };

    auto it = keywords.find(text);
    if (it != keywords.end()) {
        return Token(it->second, text, static_cast<int>(text.length()),
                     this->line);
    }

    return Token(TokenType::IDENTIFIER, text, (int)text.length(), this->line);
}
