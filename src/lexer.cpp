#include "lexer.hpp"

#include "token.hpp"
#include <unordered_map>
#include <vector>

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
            tokens.push_back(this->makeToken(TokenType::LParen));
            break;
        case ')':
            tokens.push_back(this->makeToken(TokenType::RParen));
            break;
        case '{':
            tokens.push_back(this->makeToken(TokenType::LBrace));
            break;
        case '}':
            tokens.push_back(this->makeToken(TokenType::RBrace));
            break;
        case '[':
            tokens.push_back(this->makeToken(TokenType::LBracket));
            break;
        case ']':
            tokens.push_back(this->makeToken(TokenType::RBracket));
            break;
        case ':':
            tokens.push_back(this->makeToken(TokenType::Colon));
            break;
        case ';':
            tokens.push_back(this->makeToken(TokenType::Semicolon));
            break;
        case ',':
            tokens.push_back(this->makeToken(TokenType::Comma));
            break;

        case '=':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::EqualEqual : TokenType::Equal));
            break;
        case '!':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::BangEqual : TokenType::Bang));
            break;
        case '+':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::PlusEqual : TokenType::Plus));
            break;
        case '-':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::MinusEqual : TokenType::Minus));
            break;
        case '*':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::StarEqual : TokenType::Star));
            break;
        case '/':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::SlashEqual : TokenType::Slash));
            break;
        case '%':
            tokens.push_back(this->makeToken(this->match('=')
                                                 ? TokenType::PercentEqual
                                                 : TokenType::Percent));
            break;
        case '<':
            tokens.push_back(this->makeToken(
                this->match('=') ? TokenType::LessEqual : TokenType::Less));
            break;
        case '>':
            tokens.push_back(this->makeToken(this->match('=')
                                                 ? TokenType::GreaterEqual
                                                 : TokenType::Greater));
            break;

        case '&':
            if (this->match('&')) {
                tokens.push_back(this->makeToken(TokenType::And));
            } else {
                THROW_LEXER(this->line, "Unexpected character '&'",
                            this->verbose);
            }
            break;
        case '|':
            if (this->match('|')) {
                tokens.push_back(this->makeToken(TokenType::Or));
            } else {
                THROW_LEXER(this->line, "Unexpected character '|'",
                            this->verbose);
            }
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
                            this->verbose);
            }
            break;
        }
    }

    tokens.emplace_back(TokenType::Eof, "", 0, this->line);
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

    return this->makeToken(TokenType::Number);
}

static const std::unordered_map<std::string, TokenType> keywords = {
    {"fn", TokenType::Fn},
    {"let", TokenType::Let},
    {"mut", TokenType::Mut},
    {"return", TokenType::Return},
};

Token Lexer::identifier() {
    while (std::isalnum(this->peek()) || this->peek() == '_') {
        this->advance();
    }

    std::string text = this->src.substr(this->start, this->cur - this->start);
    if (text == "true" || text == "false") {
        return Token(TokenType::Bool, text, (int)text.length(), this->line);
    }

    auto it = keywords.find(text);
    if (it != keywords.end()) {
        return Token(it->second, text, static_cast<int>(text.length()),
                     this->line);
    }

    return Token(TokenType::Identifier, text, (int)text.length(), this->line);
}
