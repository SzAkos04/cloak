#include "parser.h"

#include "ast.h"
#include "debug.h"
#include "token.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void free_ast_node(ast_node_t *node);

static parser_t parser;

void parser_init(token_t *tokens, int count) {
    parser.tokens = tokens;
    parser.count = count;
    parser.current = 0;
}

static token_t peek(void) { return parser.tokens[parser.current]; }

static token_t advance(void) { return parser.tokens[parser.current++]; }

static bool is_at_end(void) { return parser.current >= parser.count; }

static bool match(token_type_t type) {
    if (peek().type == type) {
        advance();
        return true;
    } else {
        return false;
    }
}

static int parse_expression(ast_node_t **node) {
    token_t tok = peek();
    if (tok.type == TOKEN_IDENTIFIER) {
        advance();
        ast_node_t *id = (ast_node_t *)malloc(sizeof(ast_node_t));
        if (!id) {
            perr("parser: failed to allocate memory for `id` node");
            return -1;
        }
        id->type = AST_IDENTIFIER;
        id->identifier.name = strdup(tok.lexeme);
        if (!id->identifier.name) {
            perr("failed to allocate memory for `id->identifier.name`");
            free(id);
            return -1;
        }
        *node = id;
        return 0;
    } else if (tok.type == TOKEN_NUMBER) {
        advance();
        ast_node_t *lit = (ast_node_t *)malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("parser: failed to allocate memory for `lit` node");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_NUMBER;
        char *endptr;
        lit->literal.number = strtod(tok.lexeme, &endptr);
        lit->literal.num_type = (strchr(tok.lexeme, '.') ? TYPE_F64 : TYPE_I32);
        if (endptr == tok.lexeme) {
            error("invalid number literal at line %d: %s", tok.line,
                  tok.lexeme);
            free(lit);
            return -1;
        }
        *node = lit;
        return 0;
    } else if (tok.type == TOKEN_STRING) {
        advance();
        ast_node_t *lit = (ast_node_t *)malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("parser: failed to allocate memory for `lit` node");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_STRING;
        lit->literal.string = strdup(tok.lexeme);
        if (!lit->literal.string) {
            perr("parser: failed to allocate memory for `lit->literal.string` "
                 "string");
            free(lit);
            return -1;
        }
        *node = lit;
        return 0;
    } else if (tok.type == TOKEN_BOOL) {
        advance();
        ast_node_t *lit = (ast_node_t *)malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("parser: failed to allocate memory for `lit` node");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_BOOL;
        lit->literal.boolean = (strcmp(tok.lexeme, "true") == 0);
        *node = lit;
        return 0;
    }

    error("unexpected token `%s` (type %s) at line %d while parsing expression",
          tok.lexeme, token_type_to_str(tok.type), tok.line);
    return -1;
}

// if it's not an assignment node, returns 1
// if it is returns 0
// if it fails returns -1
static int parse_assign(ast_node_t **node) {
    token_t name_tok = peek();
    if (name_tok.type != TOKEN_IDENTIFIER) {
        error("expected identifier before `=` at line %d, found "
              "`%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    // save current position to backtrack if no assignment found
    int saved_pos = parser.current;

    // consume identifier token
    advance();

    if (!match(TOKEN_EQUAL)) {
        // not an assignment, rewind parser current position and return failure
        parser.current = saved_pos;
        return 1;
    }

    ast_node_t *value = NULL;
    if (parse_expression(&value) != 0) {
        return -1;
    }

    if (!match(TOKEN_SEMICOLON)) {
        error("expected `;` after assignment at line %d, found `%s`",
              peek().line, peek().lexeme);
        free_ast_node(value);
        return -1;
    }

    ast_node_t *assign = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!assign) {
        perr("parser: failed to allocate memory for `assign` node");
        free_ast_node(value);
        return -1;
    }

    assign->type = AST_ASSIGN;
    assign->assign.name = strdup(name_tok.lexeme);
    if (!assign->assign.name) {
        perr("parser: failed to allocate memory for `assign->name` string");
        free_ast_node(value);
        free(assign);
        return -1;
    }
    assign->assign.value = value;

    *node = assign;
    return 0;
}

static int parse_type(const char *str, type_t *type) {
    if (strcmp(str, "bool") == 0) {
        *type = TYPE_BOOL;
    } else if (strcmp(str, "f32") == 0) {
        *type = TYPE_F32;
    } else if (strcmp(str, "f64") == 0) {
        *type = TYPE_F64;
    } else if (strcmp(str, "i8") == 0) {
        *type = TYPE_I8;
    } else if (strcmp(str, "i16") == 0) {
        *type = TYPE_I16;
    } else if (strcmp(str, "i32") == 0) {
        *type = TYPE_I32;
    } else if (strcmp(str, "i64") == 0) {
        *type = TYPE_I64;
    } else if (strcmp(str, "string") == 0) {
        *type = TYPE_STRING;
    } else if (strcmp(str, "void") == 0) {
        *type = TYPE_VOID;
    } else {
        error("unknown type: `%s`", str);
        return -1;
    }

    return 0;
}

static int parse_let(ast_node_t **node) {
    if (!match(TOKEN_LET)) {
        error("expected `let` keyword at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    bool is_mutable = false;
    if (match(TOKEN_MUT)) {
        is_mutable = true;
    }

    token_t name_tok = peek();
    if (!match(TOKEN_IDENTIFIER)) {
        error("expected identifier after `let` at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    type_t type = TYPE_VOID;
    if (match(TOKEN_COLON)) {
        token_t type_tok = peek();
        if (!match(TOKEN_IDENTIFIER)) {
            error("expected type after `:` at line %d, found `%s`", peek().line,
                  peek().lexeme);
            return -1;
        }
        if (parse_type(type_tok.lexeme, &type) != 0) {
            return -1;
        }
    }

    if (type == TYPE_VOID) {
        error(
            "type annotation required for `let` binding at line %d, found `%s`",
            peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t *value = NULL;
    if (match(TOKEN_EQUAL)) {
        if (parse_expression(&value) != 0) {
            return -1;
        }
    }

    if (!match(TOKEN_SEMICOLON)) {
        error("expected `;` after let binding at line %d, found `%s`",
              peek().line, peek().lexeme);
        if (value) {
            free_ast_node(value);
        }
        return -1;
    }

    ast_node_t *let = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!let) {
        perr("parser: failed to allocate memory for `let` node");
        if (value) {
            free_ast_node(value);
        }
        return -1;
    }

    let->type = AST_LET;
    let->let.name = strdup(name_tok.lexeme);
    if (!let->let.name) {
        perr("parser: failed to allocate memory for `let->let.name` string");
        free_ast_node(value);
        free(let);
        return -1;
    }
    let->let.is_mutable = is_mutable;
    let->let.type = type;
    let->let.value = value;

    *node = let;
    return 0;
}

static int parse_return(ast_node_t **node) {
    if (!match(TOKEN_RETURN)) {
        error("expected `return` keyword at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }
    ast_node_t *expr = NULL;

    // check if next token is semicolon right after return (empty return)
    if (!match(TOKEN_SEMICOLON)) {
        // if no semicolon, parse an expression
        if (parse_expression(&expr) != 0) {
            return -1;
        }

        if (!match(TOKEN_SEMICOLON)) {
            error("expected `;` after return expression at line %d, found `%s`",
                  peek().line, peek().lexeme);
            if (expr) {
                free_ast_node(expr);
            }
            return -1;
        }
    }

    ast_node_t *ret = malloc(sizeof(ast_node_t));
    if (!ret) {
        perr("parser: failed to allocate memory for `ret` node");
        if (expr) {
            free_ast_node(expr);
        }
        return -1;
    }
    ret->type = AST_RETURN;
    ret->return_stmt.value = expr;
    *node = ret;
    return 0;
}

static int parse_block(ast_node_t **node) {
    if (!match(TOKEN_LBRACE)) {
        error("expected `{` line %d, found `%s`", peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t **stmts = NULL;
    int stmt_count = 0;

    while (!match(TOKEN_RBRACE) && !is_at_end()) {
        ast_node_t *stmt;
        token_t tok = peek();
        switch (tok.type) {
        case TOKEN_LET:
            if (parse_let(&stmt) != 0) {
                return -1;
            }
            break;
        case TOKEN_RETURN:
            if (parse_return(&stmt) != 0) {
                return -1;
            }
            break;
        case TOKEN_IDENTIFIER: {
            int ret = parse_assign(&stmt);
            if (ret < 0) {
                return -1;
            } else if (ret > 0) {
                error("not yet implemented");
                return -1;
            }
            break;
        }
        default:
            error("unexpected token `%s` at line %d in block — expected `let`, "
                  "`return`, or assignment",
                  peek().lexeme, peek().line);
            return -1;
        }
        stmts = (ast_node_t **)realloc(stmts,
                                       sizeof(ast_node_t *) * (stmt_count + 1));
        if (!stmts) {
            perr("parser: failed to allocate memory for `stmts` nodes");
            return -1;
        }
        stmts[stmt_count++] = stmt;
    }

    if (is_at_end()) {
        error("unexpected end of input at line %d; `}` expected", peek().line);
        return -1;
    }

    ast_node_t *block = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!block) {
        perr("parser: failed to allocate memory for `block` node");
        return -1;
    }
    block->type = AST_BLOCK;
    block->block.stmt = stmts;
    block->block.stmt_count = stmt_count;
    *node = block;
    return 0;
}

static int parse_function(ast_node_t **node) {
    if (!match(TOKEN_FN)) {
        error(
            "expected `fn` to start function definition at line %d, found `%s`",
            peek().line, peek().lexeme);
        return -1;
    }

    token_t name = peek();
    if (!match(TOKEN_IDENTIFIER)) {
        error("expected identifier after `fn` at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    if (!match(TOKEN_LPAREN)) {
        error("expected `(` after function name at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    param_t *params = NULL;
    int param_count = 0;

    if (!match(TOKEN_RPAREN)) { // if not immediately `)`, parse params
        while (true) {
            token_t param_name_tok = peek();
            if (!match(TOKEN_IDENTIFIER)) {
                error("expected parameter name at line %d, found `%s`",
                      peek().line, peek().lexeme);
                free(params);
                return -1;
            }

            if (!match(TOKEN_COLON)) {
                error(
                    "expected `:` after parameter name at line %d, found `%s`",
                    peek().line, peek().lexeme);
                free(params);
                return -1;
            }

            token_t param_type_tok = peek();
            if (!match(TOKEN_IDENTIFIER)) {
                error("expected parameter type at line %d, found `%s`",
                      peek().line, peek().lexeme);
                free(params);
                return -1;
            }

            type_t param_type;
            if (parse_type(param_type_tok.lexeme, &param_type) != 0) {
                free(params);
                return -1;
            }

            // Add param to list
            param_t *new_params =
                realloc(params, sizeof(param_t) * (param_count + 1));
            if (!new_params) {
                perr("parser: failed to allocate memory for function params");
                free(params);
                return -1;
            }
            params = new_params;
            params[param_count].name = strdup(param_name_tok.lexeme);
            if (!params[param_count].name) {
                perr("parser: failed to allocate memory for param name string");
                for (int i = 0; i < param_count; i++) {
                    free(params[i].name);
                }
                free(params);
                return -1;
            }
            params[param_count].type = param_type;
            param_count++;

            if (match(TOKEN_COMMA)) {
                continue; // parse next param
            } else if (match(TOKEN_RPAREN)) {
                break; // end of params
            } else {
                error("expected `,` or `)` after parameter at line %d, found "
                      "`%s`",
                      peek().line, peek().lexeme);
                // free all param names and params array
                for (int i = 0; i < param_count; i++) {
                    free(params[i].name);
                }
                free(params);
                return -1;
            }
        }
    }

    if (!match(TOKEN_COLON)) {
        error("expected `:` at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }
    token_t ret = peek();
    if (!match(TOKEN_IDENTIFIER)) {
        error("expected return type at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }
    type_t ret_type;
    if (parse_type(ret.lexeme, &ret_type) != 0) {
        return -1;
    }

    ast_node_t *body;
    if (parse_block(&body) != 0) {
        return -1;
    }

    ast_node_t *fn = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!fn) {
        perr("parser: failed to allocate memory for `fn` node");
        return -1;
    }
    fn->type = AST_FUNCTION;
    fn->func.name = strdup(name.lexeme);
    if (!fn->func.name) {
        perr("parser: failed to allocate memory for `fn->func.name` string");
        free(fn);
        return -1;
    }
    fn->func.params = params;
    fn->func.param_count = param_count;
    fn->func.ret_type = ret_type;
    fn->func.body = body;
    *node = fn;
    return 0;
}

static int parse_program(ast_node_t **node) {
    ast_node_t **decls = NULL;
    int decl_count = 0;

    while (!is_at_end()) {
        ast_node_t *decl = NULL;

        if (peek().type == TOKEN_FN) {
            if (parse_function(&decl) != 0) {
                for (int i = 0; i < decl_count; i++) {
                    free_ast_node(decls[i]);
                }
                free(decls);
                return -1;
            }
        } else if (peek().type == TOKEN_EOF) {
            break;
        } else {
            error("unexpected token `%s` at line %d, expected top-level "
                  "declaration",
                  peek().lexeme, peek().line);
            for (int i = 0; i < decl_count; i++) {
                free_ast_node(decls[i]);
            }
            free(decls);
            return -1;
        }

        ast_node_t **tmp = (ast_node_t **)realloc(decls, sizeof(ast_node_t *) *
                                                             (decl_count + 1));
        if (!tmp) {
            perr("parser: failed to allocate memory for declarations");
            for (int i = 0; i < decl_count; i++) {
                free_ast_node(decls[i]);
            }
            free(decls);
            free_ast_node(decl);
            return -1;
        }
        decls = tmp;
        decls[decl_count++] = decl;
    }

    ast_node_t *program = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!program) {
        perr("parser: failed to allocate memory for program node");
        for (int i = 0; i < decl_count; i++) {
            free_ast_node(decls[i]);
        }
        free(decls);
        return -1;
    }

    program->type = AST_PROGRAM;
    program->program.decls = decls;
    program->program.decl_count = decl_count;

    *node = program;
    return 0;
}

int parse_ast(ast_t **ast) {
    ast_node_t *root; // for now, one top-level function
    if (parse_program(&root) != 0) {
        return -1;
    }

    *ast = (ast_t *)malloc(sizeof(ast_t));
    if (!*ast) {
        perr("Failed to allocate abstract syntax tree");
        return -1;
    }
    (*ast)->root = root;
    return 0;
}

void free_ast_node(ast_node_t *node) {
    if (!node) {
        return;
    }

    switch (node->type) {
    case AST_PROGRAM:
        for (int i = 0; i < node->program.decl_count; i++) {
            free_ast_node(node->program.decls[i]);
        }
        free(node->program.decls);
        break;
    case AST_IDENTIFIER:
        free(node->identifier.name);
        break;

    case AST_LITERAL:
        if (node->literal.kind == LITERAL_STRING) {
            free(node->literal.string);
        }
        break;

    case AST_BLOCK:
        for (int i = 0; i < node->block.stmt_count; i++) {
            free_ast_node(node->block.stmt[i]);
        }
        free(node->block.stmt);
        break;

    case AST_ASSIGN:
        free(node->assign.name);
        free_ast_node(node->assign.value);
        break;

    case AST_FUNCTION:
        free(node->func.name);
        for (int i = 0; i < node->func.param_count; i++) {
            free(node->func.params[i].name);
        }
        free(node->func.params);
        free_ast_node(node->func.body);
        break;

    case AST_LET:
        free(node->let.name);
        free_ast_node(node->let.value);
        break;

    case AST_RETURN:
        free_ast_node(node->return_stmt.value);
        break;

    default:
        break;
    }

    free(node);
}

void free_ast(ast_t *ast) {
    if (!ast) {
        return;
    }
    free_ast_node(ast->root);
    free(ast);
}
