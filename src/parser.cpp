#include "parser.hpp"

#include "ast.hpp"
#include "token.hpp"

#include <fmt/core.h>
#include <memory>
#include <utility>

std::unique_ptr<AstProgram> Parser::parseProgram() {
    std::vector<AstNodePtr> decls;
    while (!this->isAtEnd()) {
        decls.push_back(this->parseDecl());
    }
    return std::make_unique<AstProgram>(std::move(decls));
}

bool Parser::isAtEnd() const {
    return this->current >= this->tokens.size() ||
           this->peek().getType() == TokenType::Eof;
}

const Token &Parser::peek() const { return this->tokens.at(this->current); }

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
    if (this->check(TokenType::Fn)) {
        return this->parseFn();
    } else if (this->check(TokenType::Let)) {
        return this->parseLet();
    }

    THROW_PARSER(this->peek().getLine(), "Expected declaration", this->verbose);
    return nullptr;
}

AstNodePtr Parser::parseStmt() {
    Token tok = this->peek();
    switch (tok.getType()) {
    case TokenType::Let:
        return this->parseLet();
    case TokenType::Return:
        return this->parseReturn();
    default:
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("`{}` not yet implemented", this->peek().getLexeme()),
            this->verbose);
    }
}

Type Parser::parseType() {
    std::string lexeme = this->peek().getLexeme();
    if (this->check(TokenType::Identifier)) {
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
                         fmt::format("Expected type name, found `{}`", lexeme),
                         this->verbose);
        }

    } else {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected type name, found `{}`", lexeme),
                     this->verbose);
    }
}

Type Parser::parseArr() {
    if (this->peek().getLexeme() != "arr") {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `arr`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    }
    this->advance(); // consume `arr`

    if (!this->match(TokenType::Less)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected `<` after `arr`, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    }
    // `<` is consumed

    Type elementType = this->parseType();
    std::unique_ptr<Type> elementTypePtr =
        std::make_unique<Type>(std::move(elementType));

    if (!this->match(TokenType::Comma)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `,`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    }
    // `,` is consumed

    AstNodePtr expr = this->parseExpr();

    if (!this->match(TokenType::Greater)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `>`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    }

    Type arr(std::move(elementTypePtr), std::move(expr));

    return arr;
}

AstNodePtr Parser::parseExpr() {
    auto lhs = this->parseUnaryExpr();
    if (!lhs) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected expression, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    }
    return this->parseBinaryRhs(0, std::move(lhs));
}

AstNodePtr Parser::parsePrimary() {
    const Token &tok = this->peek();

    switch (tok.getType()) {
    case TokenType::LParen: {
        this->advance(); // consume '('
        auto expr = this->parseExpr();
        if (!expr) {
            THROW_PARSER(this->peek().getLine(),
                         fmt::format("Expected expression, found `{}`",
                                     this->peek().getLexeme()),
                         this->verbose);
        }

        if (!this->match(TokenType::RParen)) {
            THROW_PARSER(
                tok.getLine(),
                fmt::format("Expected ')' after expression, found `{}`",
                            this->peek().getLexeme()),
                this->verbose);
        }
        return expr;
    }

    case TokenType::Identifier: {
        this->advance();
        return std::make_unique<AstIdentifier>(tok.getLexeme());
    }

    case TokenType::Number: {
        this->advance();
        // convert to double
        try {
            double val = std::stod(tok.getLexeme());
            PrimaryType type =
                std::floor(val) == val ? PrimaryType::I32 : PrimaryType::F64;
            return std::make_unique<AstLiteral>(
                Literal(NumberLiteral(val, type)));
        } catch (...) {
            THROW_PARSER(
                tok.getLine(),
                fmt::format("Invalid number literal `{}`", tok.getLexeme()),
                this->verbose);
        }
    }

    case TokenType::String: {
        this->advance();
        return std::make_unique<AstLiteral>(Literal(tok.getLexeme()));
    }

    case TokenType::Bool: {
        this->advance();
        return std::make_unique<AstLiteral>(Literal(tok.getLexeme() == "true"));
    }

    default:
        THROW_PARSER(
            tok.getLine(),
            fmt::format(
                "Unexpected token `{}` while parsing primary expression",
                tok.getLexeme()),
            this->verbose);
    }
}

AstNodePtr Parser::parseUnaryExpr() {
    const Token &tok = this->peek();
    if (tok.getType() == TokenType::Bang || tok.getType() == TokenType::Minus) {
        UnaryOp op = tokenTypeToUnaryOp(tok.getType());
        this->advance(); // consume operator
        auto operand = this->parseUnaryExpr();
        if (!operand) {
            return nullptr;
        }
        return std::make_unique<AstUnary>(op, std::move(operand));
    }
    return this->parsePrimary();
}

AstNodePtr Parser::parseBinaryRhs(int precedence, AstNodePtr lhs) {
    while (true) {
        const Token &tok = this->peek();
        if (!isBinaryOp(tok.getType())) {
            return lhs;
        }

        int tokPrec = this->getPrecedence(tok.getType());

        // if the next operator has lower precedence, stop parsing binary rhs
        if (tokPrec < precedence) {
            return lhs;
        }

        BinaryOp op = tokenTypeToBinaryOp(tok.getType());
        this->advance(); // consume operator

        // parse the right hand side expression with higher precedence
        auto rhs = this->parseUnaryExpr();
        if (!rhs) {
            return nullptr;
        }

        const Token &nextTok = this->peek();
        int nextPrec = this->getPrecedence(nextTok.getType());
        if (tokPrec < nextPrec) {
            rhs = this->parseBinaryRhs(tokPrec + 1, std::move(rhs));
            if (!rhs) {
                return nullptr;
            }
        }

        lhs = std::make_unique<AstBinary>(std::move(lhs), op, std::move(rhs));
    }
}

int Parser::getPrecedence(TokenType type) const {
    switch (type) {
    case TokenType::Star:  // multiplication
    case TokenType::Slash: // division
        return 6;
    case TokenType::Plus:  // addition
    case TokenType::Minus: // subtraction
        return 5;
    case TokenType::Less:
    case TokenType::LessEqual:
    case TokenType::Greater:
    case TokenType::GreaterEqual:
        return 4;
    case TokenType::EqualEqual:
    case TokenType::BangEqual:
        return 3;
    case TokenType::And:
        return 2;
    case TokenType::Or:
        return 1;
    default:
        return -1; // not a binary operator
    }
}

AstNodePtr Parser::parseBlock() {
    if (!this->match(TokenType::LBrace)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `{`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `{` consumed

    std::vector<AstNodePtr> stmts;

    while (!this->match(TokenType::RBrace) || !this->isAtEnd()) {
        stmts.push_back(this->parseStmt());
    } // `}` consumed

    return std::make_unique<AstBlock>(std::move(stmts));
}

AstNodePtr Parser::parseFn() {
    if (!this->match(TokenType::Fn)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected `fn` keyword, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    } // `fn` consumed

    Token nameTok = peek();
    if (!this->match(TokenType::Identifier)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected identifier, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    } // identifier consumed

    if (!this->match(TokenType::LParen)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `(`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `(` consumed

    std::vector<Param> params;

    if (!this->match(TokenType::RParen)) {
        while (true) {
            Token paramNameTok = this->peek();
            if (!this->match(TokenType::Identifier)) {
                THROW_PARSER(this->peek().getLine(),
                             fmt::format("Expected parameter name, found `{}`",
                                         this->peek().getLexeme()),
                             this->verbose);
            } // identifier consumed

            if (!this->match(TokenType::Colon)) {
                THROW_PARSER(
                    this->peek().getLine(),
                    fmt::format("Expected `:` after parameter name, found `{}`",
                                this->peek().getLexeme()),
                    this->verbose);
            } // `:` consumed

            Type paramType = this->parseType(); // parameter type consumed

            params.push_back(
                Param(paramNameTok.getLexeme(), std::move(paramType)));

            if (this->match(TokenType::Comma)) {
                continue; // continue parsing
            } // possible `,` consumed
            else if (this->match(TokenType::RParen)) {
                break; // stop parsing params
            } // `)` consumed
            else {
                THROW_PARSER(
                    this->peek().getLine(),
                    fmt::format(
                        "Expected `,` or `)` after parameter, found `{}`",
                        this->peek().getLexeme()),
                    this->verbose);
            }
        }
    } // `)` consumed

    if (!this->match(TokenType::Colon)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `:`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `:` consumed

    Type retType = this->parseType(); // type consumed

    AstNodePtr body = this->parseBlock(); // block consumed

    return std::make_unique<AstFn>(nameTok.getLexeme(), std::move(params),
                                   std::move(retType), std::move(body));
}

AstNodePtr Parser::parseLet() {
    if (!this->match(TokenType::Let)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected `let` keyword, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    } // `let` consumed

    bool mut = false;
    mut = this->match(TokenType::Mut); // possible `mut` consumed

    Token nameTok = peek();
    if (!this->match(TokenType::Identifier)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected identifier, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    } // identifier consumed

    if (!this->match(TokenType::Colon)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `:`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `:` consumed

    Type type = this->parseType(); // type consumed

    if (this->match(TokenType::Semicolon)) {
        return std::make_unique<AstLet>(mut, nameTok.getLexeme(),
                                        std::move(type), nullptr);
    } // possible `;` consumed
    else if (!this->match(TokenType::Equal)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `=` or `;` after let statement, found `{}`",
                        this->peek().getLexeme()),
            this->verbose);
    } // `=` consumed

    AstNodePtr expr = this->parseExpr(); // expr consumed

    if (!this->match(TokenType::Semicolon)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `;`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `;` consumed

    return std::make_unique<AstLet>(mut, nameTok.getLexeme(), std::move(type),
                                    std::move(expr));
}

AstNodePtr Parser::parseReturn() {
    if (!this->match(TokenType::Return)) {
        THROW_PARSER(this->peek().getLine(),
                     fmt::format("Expected `return` keyword, found `{}`",
                                 this->peek().getLexeme()),
                     this->verbose);
    } // `return` consumed

    if (this->match(TokenType::Semicolon)) {
        return std::make_unique<AstReturn>(nullptr);
    }

    AstNodePtr expr = this->parseExpr(); // expr consumed

    if (!this->match(TokenType::Semicolon)) {
        THROW_PARSER(
            this->peek().getLine(),
            fmt::format("Expected `;`, found `{}`", this->peek().getLexeme()),
            this->verbose);
    } // `;` consumed

    return std::make_unique<AstReturn>(std::move(expr));
}
