#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"

#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

ParserError::ParserError(int line, const std::string &msg, bool verbose,
                         const char *file, int line_, const char *func)
    : std::runtime_error(
          this->formatMessage(line, msg, verbose, file, line_, func)) {}

std::string ParserError::formatMessage(int line, const std::string &msg,
                                       bool verbose, const char *file,
                                       int line_, const char *func) {
    std::ostringstream oss;
    if (!verbose || file == nullptr || func == nullptr || line == 0) {
        oss << "Line " << std::to_string(line) << ": " << msg;
    } else {
        oss << file << ":" << line_ << " (" << func << "): " << "Line "
            << std::to_string(line) << ": " << msg;
    }
    return oss.str();
}

bool verbose;

Parser::Parser(const std::vector<Token> &tokens, bool verb)
    : tokens(tokens), current(0), verbose(verb) {}

std::unique_ptr<AstProgram> Parser::parseProgram() {
    std::vector<AstNodePtr> decls;
    while (!this->isAtEnd()) {
        decls.push_back(this->parseDecl());
    }
    return std::make_unique<AstProgram>(std::move(decls));
}

bool Parser::isAtEnd() const {
    return this->current >= this->tokens.size() ||
           this->peek().getType() == TokenType::EOF_;
}

const Token &Parser::peek() const { return this->tokens.at(this->current); }

const Token &Parser::previous() const {
    return this->tokens.at(this->current - 1);
}

const Token &Parser::advance() { return this->tokens.at(this->current++); }

bool Parser::check(TokenType type) const {
    if (this->isAtEnd()) {
        return false;
    }
    return this->peek().getType() == type;
}

bool Parser::match(TokenType type) {
    if (this->peek().getType() == type) {
        this->advance();
        return true;
    } else {
        return false;
    }
}

AstNodePtr Parser::parseDecl() {
    if (this->check(TokenType::FN)) {
        return parseFn();
    }

    THROW_PARSER(this->peek().getLine(), "Expected declaration", this->verbose);
    return nullptr;
}

AstNodePtr Parser::parseStmt() {
    Token tok = this->peek();
    switch (tok.getType()) {
    default:
        THROW_PARSER(this->peek().getLine(),
                     "`" + this->peek().getLexeme() + "` not yet implemented",
                     this->verbose);
    }
}

Type Parser::parseType() {
    std::string lexeme = this->peek().getLexeme();
    if (check(TokenType::IDENTIFIER)) {
        // parse array type
        if (lexeme == "arr") {
            return this->parseArr();
        } else if (lexeme == "bool") {
            this->advance();
            return Type(PrimaryType::Bool);
        } else if (lexeme == "i8") {
            this->advance();
            return Type(PrimaryType::I8);
        } else if (lexeme == "i16") {
            this->advance();
            return Type(PrimaryType::I16);
        } else if (lexeme == "i32") {
            this->advance();
            return Type(PrimaryType::I32);
        } else if (lexeme == "i64") {
            this->advance();
            return Type(PrimaryType::I64);
        } else if (lexeme == "u8") {
            this->advance();
            return Type(PrimaryType::U8);
        } else if (lexeme == "u16") {
            this->advance();
            return Type(PrimaryType::U16);
        } else if (lexeme == "u32") {
            this->advance();
            return Type(PrimaryType::U32);
        } else if (lexeme == "u64") {
            this->advance();
            return Type(PrimaryType::U64);
        } else if (lexeme == "f32") {
            this->advance();
            return Type(PrimaryType::F32);
        } else if (lexeme == "f64") {
            this->advance();
            return Type(PrimaryType::F64);
        } else if (lexeme == "string") {
            this->advance();
            return Type(PrimaryType::String);
        } else if (lexeme == "void") {
            this->advance();
            return Type(PrimaryType::Void);
        } else {
            THROW_PARSER(this->peek().getLine(),
                         "Expected type name, found `" + lexeme + "`",
                         this->verbose);
        }

    } else {
        THROW_PARSER(this->peek().getLine(),
                     "Expected type name, found `" + lexeme + "`",
                     this->verbose);
    }
}

Type Parser::parseArr() {
    if (this->peek().getLexeme() != "arr") {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `arr`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    }
    this->advance(); // consume `arr`

    if (!this->match(TokenType::LESS)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `<` after `arr`, found `" +
                         this->peek().getLexeme() + "`",
                     this->verbose);
    }
    // `<` is consumed

    Type element_type = parseType();
    std::unique_ptr<Type> element_type_ptr =
        std::make_unique<Type>(std::move(element_type));

    if (!this->match(TokenType::COMMA)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `,`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    }
    // `,` is consumed

    AstNodePtr expr = this->parseExpr();

    if (!this->match(TokenType::GREATER)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `>`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    }

    Type arr(std::move(element_type_ptr), std::move(expr));

    return arr;
}

AstNodePtr Parser::parseExpr() {
    auto lhs = parseUnaryExpr();
    if (!lhs) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected expression, found `" + this->peek().getLexeme() +
                         "`",
                     this->verbose);
    }
    return parseBinaryRhs(0, std::move(lhs));
}

AstNodePtr Parser::parsePrimary() {
    const Token &tok = this->peek();

    switch (tok.getType()) {
    case TokenType::LPAREN: {
        this->advance(); // consume '('
        auto expr = this->parseExpr();
        if (!expr) {
            THROW_PARSER(this->peek().getLine(),
                         "Expected expression, found `" +
                             this->peek().getLexeme() + "`",
                         this->verbose);
        }

        if (!this->match(TokenType::RPAREN)) {
            THROW_PARSER(tok.getLine(), "Expected ')' after expression",
                         this->verbose);
        }
        return expr;
    }

    case TokenType::IDENTIFIER: {
        if (tok.getLexeme() == "true" || tok.getLexeme() == "false") {
            this->advance();
            return std::make_unique<AstLiteral>(tok.getLexeme() == "true");
        } else {
            this->advance();
            return std::make_unique<AstIdentifier>(tok.getLexeme());
        }
    }

    case TokenType::NUMBER: {
        this->advance();
        // convert to double
        try {
            double val = std::stod(tok.getLexeme());
            return std::make_unique<AstLiteral>(val);
        } catch (...) {
            THROW_PARSER(tok.getLine(),
                         "Invalid number literal: " + tok.getLexeme(), verbose);
        }
    }

    case TokenType::STRING: {
        this->advance();
        return std::make_unique<AstLiteral>(tok.getLexeme());
    }

    default:
        THROW_PARSER(tok.getLine(),
                     "Unexpected token `" + tok.getLexeme() +
                         "` while parsing primary expression",
                     verbose);
    }
}

AstNodePtr Parser::parseUnaryExpr() {
    const Token &tok = peek();
    if (tok.getType() == TokenType::BANG || tok.getType() == TokenType::MINUS) {
        UnaryOp op = tokenTypeToUnaryOp(tok.getType());
        advance(); // consume operator
        auto operand = parseUnaryExpr();
        if (!operand) {
            return nullptr;
        }
        return std::make_unique<AstUnary>(op, std::move(operand));
    }
    return parsePrimary();
}

AstNodePtr Parser::parseBinaryRhs(int precedence, AstNodePtr lhs) {
    while (true) {
        const Token &tok = peek();
        if (!isBinaryOp(tok.getType())) {
            return lhs;
        }

        int tokPrec = getPrecedence(tok.getType());

        // If the next operator has lower precedence, stop parsing binary rhs
        if (tokPrec < precedence) {
            return lhs;
        }

        BinaryOp op = tokenTypeToBinaryOp(tok.getType());
        advance(); // consume operator

        // parse the right hand side expression with higher precedence
        auto rhs = parseUnaryExpr();
        if (!rhs) {
            return nullptr;
        }

        const Token &nextTok = peek();
        int nextPrec = getPrecedence(nextTok.getType());
        if (tokPrec < nextPrec) {
            rhs = parseBinaryRhs(tokPrec + 1, std::move(rhs));
            if (!rhs) {
                return nullptr;
            }
        }

        // Create a binary expression node combining lhs and rhs
        lhs = std::make_unique<AstBinary>(std::move(lhs), op, std::move(rhs));
    }
}

int Parser::getPrecedence(TokenType type) const {
    switch (type) {
    case TokenType::STAR:  // multiplication
    case TokenType::SLASH: // division
        return 6;
    case TokenType::PLUS:  // addition
    case TokenType::MINUS: // subtraction
        return 5;
    case TokenType::LESS:
    case TokenType::LESSEQUAL:
    case TokenType::GREATER:
    case TokenType::GREATEREQUAL:
        return 4;
    case TokenType::EQUALEQUAL:
    case TokenType::BANGEQUAL:
        return 3;
    case TokenType::AND:
        return 2;
    case TokenType::OR:
        return 1;
    default:
        return -1; // not a binary operator
    }
}

AstNodePtr Parser::parseBlock() {
    if (!this->match(TokenType::LBRACE)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `{`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    } // `{` consumed

    std::vector<AstNodePtr> stmts_;

    while (!this->match(TokenType::RBRACE) || !this->isAtEnd()) {
        stmts_.push_back(parseStmt());
    } // `}` consumed

    return std::make_unique<AstBlock>(std::move(stmts_));
}

AstNodePtr Parser::parseFn() {
    if (!this->match(TokenType::FN)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `fn` keyword, found `" +
                         this->peek().getLexeme() + "`",
                     this->verbose);
    } // `fn` consumed

    Token name_tok = peek();
    if (!this->match(TokenType::IDENTIFIER)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected identifier, found `" + this->peek().getLexeme() +
                         "`",
                     this->verbose);
    } // identifier consumed

    if (!this->match(TokenType::LPAREN)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `(`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    } // `(` consumed

    std::vector<Param> params;

    if (!this->match(TokenType::RPAREN)) {
        while (true) {
            Token param_name_tok = this->peek();
            if (!this->match(TokenType::IDENTIFIER)) {
                THROW_PARSER(this->peek().getLine(),
                             "Expected parameter name, found `" +
                                 this->peek().getLexeme() + "`",
                             this->verbose);
            } // identifier consumed

            if (!this->match(TokenType::COLON)) {
                THROW_PARSER(this->peek().getLine(),
                             "Expected `:` after parameter name, found `" +
                                 this->peek().getLexeme() + "`",
                             this->verbose);
            } // `:` consumed

            Type param_type = parseType();

            params.push_back(
                Param(param_name_tok.getLexeme(), std::move(param_type)));

            if (this->match(TokenType::COMMA)) {
                continue; // continue parsing
            } else if (this->match(TokenType::RPAREN)) {
                break; // stop parsing params
            } else {
                THROW_PARSER(this->peek().getLine(),
                             "Expected `,` or `)` after parameter, found `" +
                                 this->peek().getLexeme() + "`",
                             this->verbose);
            }
        }
    } // `)` consumed

    if (!this->match(TokenType::COLON)) {
        THROW_PARSER(this->peek().getLine(),
                     "Expected `:`, found `" + this->peek().getLexeme() + "`",
                     this->verbose);
    }

    Type ret_type = this->parseType();

    AstNodePtr body = this->parseBlock();

    return std::make_unique<AstFn>(name_tok.getLexeme(), std::move(params),
                                   std::move(ret_type), std::move(body));
}
