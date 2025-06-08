#include "ast.h"
#include "debug.h"
#include "parser.h"
#include "token.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

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
            perr("failed to allocate memory for `id`");
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
        ast_node_t *lit = malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("failed to allocate memory for `lit`");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_NUMBER;
        char *endptr;
        lit->literal.number = strtod(tok.lexeme, &endptr);
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
        ast_node_t *lit = malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("failed to allocate memory for `lit`");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_STRING;
        lit->literal.string = strdup(tok.lexeme);
        if (!lit->literal.string) {
            perr("failed to allocate memory for `lit->literal.string`");
            free(lit);
            return -1;
        }
        *node = lit;
        return 0;
    } else if (tok.type == TOKEN_BOOL) {
        advance();
        ast_node_t *lit = malloc(sizeof(ast_node_t));
        if (!lit) {
            perr("failed to allocate memory for `lit`");
            return -1;
        }
        lit->type = AST_LITERAL;
        lit->literal.kind = LITERAL_BOOL;
        lit->literal.boolean = (strcmp(tok.lexeme, "true") == 0);
        *node = lit;
        return 0;
    }

    error("unexpected expression at line %d: `%s`", tok.line, tok.lexeme);
    return -1;
}

static int parse_return(ast_node_t **node) {
    if (!match(TOKEN_RETURN)) {
        error("`return` keyword expected");
        return -1;
    }
    ast_node_t *expr;
    if (parse_expression(&expr) != 0) {
        return -1;
    }
    if (!match(TOKEN_SEMICOLON)) {
        error("`;` expected");
        return -1;
    }

    ast_node_t *ret = malloc(sizeof(ast_node_t));
    if (!ret) {
        perr("failed to allocate memory for `ret`");
        return -1;
    }
    ret->type = AST_RETURN;
    ret->return_stmt.value = expr;
    *node = ret;
    return 0;
}

static int parse_block(ast_node_t **node) {
    if (!match(TOKEN_LBRACE)) {
        error("`{` expected");
        return -1;
    }

    ast_node_t **stmts = NULL;
    int stmt_count = 0;

    while (!match(TOKEN_RBRACE) && !is_at_end()) {
        ast_node_t *stmt;
        if (parse_return(&stmt) != 0) {
            return -1;
        }
        stmts = realloc(stmts, sizeof(ast_node_t *) * (stmt_count + 1));
        if (!stmts) {
            perr("failed to allocate memory for `stmts`");
            return -1;
        }
        stmts[stmt_count++] = stmt;
    }

    if (is_at_end()) {
        error("unexpected end of input; `}` expected");
        return -1;
    }

    ast_node_t *block = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!block) {
        perr("failed to allocate memory for `block`");
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
        error("`fn` expected");
        return -1;
    }

    token_t name = peek();
    if (!match(TOKEN_IDENTIFIER)) {
        error("function name expected");
        return -1;
    }

    if (!match(TOKEN_LPAREN)) {
        error("`(` expected");
        return -1;
    }
    if (!match(TOKEN_RPAREN)) {
        error("`)` expected");
        return -1;
    }

    if (!match(TOKEN_COLON)) {
        error("`:` expected");
        return -1;
    }
    if (!match(TOKEN_IDENTIFIER)) { // skip for now
        error("return type expected");
        return -1;
    }

    ast_node_t *body;
    if (parse_block(&body) != 0) {
        return -1;
    }

    ast_node_t *fn = malloc(sizeof(ast_node_t));
    if (!fn) {
        perr("failed to allocate memory for `fn`");
        return -1;
    }
    fn->type = AST_FUNCTION;
    fn->func.name = strdup(name.lexeme);
    if (!fn->func.name) {
        perr("failed to allocate memory for `fn->func.name`");
        return -1;
    }
    fn->func.params = NULL; // no params yet
    fn->func.param_count = 0;
    fn->func.body = body;
    *node = fn;
    return 0;
}

int parse_ast(ast_t **ast) {
    ast_node_t *root; // for now, one top-level function
    if (parse_function(&root) != 0) {
        return -1;
    }

    *ast = (ast_t *)malloc(sizeof(ast_t));
    if (!*ast) {
        perr("Failed to allocate ast");
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

    case AST_FUNCTION:
        free(node->func.name);
        for (int i = 0; i < node->func.param_count; i++) {
            free_ast_node(node->func.params[i]);
        }
        free(node->func.params);
        free_ast_node(node->func.body);
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

static void debug_ast_node(ast_node_t *node, int indent) {
    if (!node)
        return;
    for (int i = 0; i < indent; i++)
        printf("  ");
    switch (node->type) {
    case AST_IDENTIFIER:
        printf("AST_IDENTIFIER: %s\n", node->identifier.name);
        break;
    case AST_LITERAL:
        switch (node->literal.kind) {
        case LITERAL_NUMBER:
            printf("AST_LITERAL (number): %f\n", node->literal.number);
            break;
        case LITERAL_STRING:
            printf("AST_LITERAL (string): \"%s\"\n", node->literal.string);
            break;
        case LITERAL_BOOL:
            printf("AST_LITERAL (bool): %s\n",
                   node->literal.boolean ? "true" : "false");
            break;
        default:
            printf("AST_LITERAL (unknown kind)\n");
        }
        break;
    case AST_RETURN:
        printf("AST_RETURN:\n");
        debug_ast_node(node->return_stmt.value, indent + 1);
        break;
    case AST_BLOCK:
        printf("AST_BLOCK with %d statements:\n", node->block.stmt_count);
        for (int i = 0; i < node->block.stmt_count; i++) {
            debug_ast_node(node->block.stmt[i], indent + 1);
        }
        break;
    case AST_FUNCTION:
        printf("AST_FUNCTION: %s, params=%d\n", node->func.name,
               node->func.param_count);
        if (node->func.param_count > 0) {
            printf("Params:\n");
            for (int i = 0; i < node->func.param_count; i++) {
                debug_ast_node(node->func.params[i], indent + 1);
            }
        }
        printf("Body:\n");
        debug_ast_node(node->func.body, indent + 1);
        break;
    default:
        printf("AST_UNKNOWN_TYPE: %d\n", node->type);
    }
}

void debug_ast(ast_t *ast) { debug_ast_node(ast->root, 0); }
