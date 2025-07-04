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

static int parse_expr(ast_node_t **node, bool stop_at_gt);

static int parse_type(type_t *out_type) {
    token_t tok = peek();

    if (tok.type == TOKEN_IDENTIFIER && strcmp(tok.lexeme, "arr") == 0) {
        advance(); // consume 'arr'

        if (!match(TOKEN_LESS)) {
            error("expected `<` after `arr`");
            return -1;
        }

        // Parse the element type
        type_t element_type;
        if (parse_type(&element_type) != 0) {
            return -1;
        }

        if (!match(TOKEN_COMMA)) {
            error("expected `,` in array type");
            return -1;
        }

        debug("%d", parser.current);
        debug("%s", parser.tokens[parser.current].lexeme);

        // Parse the length expression (this is a regular expression)
        ast_node_t *len_expr = NULL;
        if (parse_expr(&len_expr, true) != 0) {
            return -1;
        }
        debug("%d", parser.current);
        debug("%s", parser.tokens[parser.current].lexeme);

        if (!match(TOKEN_GREATER)) {
            error("expected `>` to close array type at line %d, found `%s`",
                  peek().line, peek().lexeme);
            return -1;
        }

        // Allocate and populate the array type
        type_t *element_type_ptr = malloc(sizeof(type_t));
        if (!element_type_ptr) {
            perr("parse_type: failed to allocate memory for element_type");
            return -1;
        }
        *element_type_ptr = element_type;

        *out_type = (type_t){
            .kind = TYPE_ARRAY,
            .data.array =
                {
                    .type = element_type_ptr,
                    .len = len_expr,
                },
        };
        return 0;
    }

    // Otherwise: parse a primary type
    if (tok.type == TOKEN_IDENTIFIER) {
        advance();
        primary_type_t ptype;
        if (strcmp(tok.lexeme, "bool") == 0) {
            ptype = TYPE_BOOL;
        } else if (strcmp(tok.lexeme, "f32") == 0) {
            ptype = TYPE_F32;
        } else if (strcmp(tok.lexeme, "f64") == 0) {
            ptype = TYPE_F64;
        } else if (strcmp(tok.lexeme, "i8") == 0) {
            ptype = TYPE_I8;
        } else if (strcmp(tok.lexeme, "i16") == 0) {
            ptype = TYPE_I16;
        } else if (strcmp(tok.lexeme, "i32") == 0) {
            ptype = TYPE_I32;
        } else if (strcmp(tok.lexeme, "i64") == 0) {
            ptype = TYPE_I64;
        } else if (strcmp(tok.lexeme, "string") == 0) {
            ptype = TYPE_STRING;
        } else if (strcmp(tok.lexeme, "void") == 0) {
            ptype = TYPE_VOID;
        } else {
            error("unknown primary type `%s`", tok.lexeme);
            return -1;
        }

        *out_type = (type_t){.kind = TYPE_PRIMARY, .data.primary = ptype};
        return 0;
    }

    error("expected type name, found `%s`", tok.lexeme);
    return -1;
}

static int parse_call(ast_node_t **node);
static int parse_index(ast_node_t **node);

static int parse_postfix(ast_node_t **node) {
    while (true) {
        token_t tok = peek();

        if (tok.type == TOKEN_LBRACKET) {
            parser.current--; // so the functions know the identifier
            if (parse_index(node) != 0) {
                return -1;
            }
        } else if (tok.type == TOKEN_LPAREN) {
            parser.current--; // so the functions know the identifier
            if (parse_call(node) != 0) {
                return -1;
            }
        } else {
            break; // no more postfixes
        }
    }
    return 0;
}

static int parse_primary(ast_node_t **node) {
    token_t tok = peek();

    if (tok.type == TOKEN_LPAREN) {
        advance(); // consume '('
        if (parse_expr(node, false) != 0)
            return -1;
        if (!match(TOKEN_RPAREN)) {
            error("expected ')' after expression at line %d, found `%s`",
                  tok.line, tok.lexeme);
            return -1;
        }
        return 0;
    }

    if (tok.type == TOKEN_IDENTIFIER) {
        advance();

        ast_node_t *id = malloc(sizeof(ast_node_t));
        if (!id) {
            perr("parser: failed to allocate memory for id node");
            return -1;
        }
        id->type = AST_IDENTIFIER;
        id->identifier.name = strdup(tok.lexeme);
        if (!id->identifier.name) {
            perr("failed to allocate memory for id->identifier.name");
            free(id);
            return -1;
        }

        *node = id;

        // Now parse postfix expressions (indexing, calls, etc.)
        return parse_postfix(node);
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
    } else {
        if (peek().type == TOKEN_SEMICOLON || peek().type == TOKEN_EQUAL) {
            return 0;
        }
        error("unexpected token `%s` (type %s) at line %d while parsing "
              "expression",
              tok.lexeme, token_type_to_str(tok.type), tok.line);
        return -1;
    }
}

static int parse_unary(ast_node_t **node) {
    token_t tok = peek();
    unary_op_t op;

    if (is_unary_op(tok)) {
        if (str_to_unary_op(tok.lexeme, &op) != 0) {
            return -1;
        }
        advance(); // consume the operator
        ast_node_t *right = NULL;
        if (parse_unary(&right) != 0) {
            return -1;
        }

        ast_node_t *unary = malloc(sizeof(ast_node_t));
        if (!unary) {
            perr("parser: failed to allocate memory for `unary` node");
            return -1;
        }

        unary->type = AST_UNARY;
        unary->unary.op = op;
        unary->unary.right = right;
        *node = unary;
        return 0;
    }

    return parse_primary(node);
}

static int get_binary_precedence(binary_op_t op) {
    switch (op) {
    case BIN_MUL:
    case BIN_DIV:
        return 6;
    case BIN_ADD:
    case BIN_SUB:
        return 5;
    case BIN_LT:
    case BIN_LTE:
    case BIN_GT:
    case BIN_GTE:
        return 4;
    case BIN_EQ:
    case BIN_NEQ:
        return 3;
    case BIN_AND:
        return 2;
    case BIN_OR:
        return 1;
    default:
        return -1;
    }
}

static int parse_binary_rhs(int min_prec, ast_node_t *lhs, ast_node_t **node,
                            bool stop_at_gt) {
    while (true) {
        token_t tok = peek();
        binary_op_t op;

        if (!is_binary_op(tok) || str_to_binary_op(tok.lexeme, &op) != 0) {
            break;
        }

        if (stop_at_gt && tok.type == TOKEN_GREATER) {
            break;
        }

        int prec = get_binary_precedence(op);
        if (prec < min_prec) {
            break;
        }

        advance(); // consume operator

        ast_node_t *rhs = NULL;
        if (parse_unary(&rhs) != 0) {
            return -1;
        }

        token_t next = peek();
        binary_op_t next_op;
        while (is_binary_op(next) &&
               str_to_binary_op(next.lexeme, &next_op) == 0 &&
               get_binary_precedence(next_op) > prec) {
            if (parse_binary_rhs(get_binary_precedence(next_op), rhs, &rhs,
                                 false) != 0)
                return -1;
            next = peek();
        }

        ast_node_t *binary = (ast_node_t *)malloc(sizeof(ast_node_t));
        if (!binary) {
            perr("parser: failed to allocate memory for `binary` node");
            return -1;
        }

        binary->type = AST_BINARY;
        binary->binary.left = lhs;
        binary->binary.op = op;
        binary->binary.right = rhs;
        lhs = binary;
    }

    *node = lhs;
    return 0;
}

static int parse_expr(ast_node_t **node, bool stop_at_gt) {
    ast_node_t *lhs = NULL;
    if (parse_unary(&lhs) != 0) {
        return -1;
    }
    return parse_binary_rhs(0, lhs, node, stop_at_gt);
}

static int parse_assignable(ast_node_t **node);

// if it's not an assignment node, returns 1
// if it is returns 0
// if it fails returns -1
static int parse_call(ast_node_t **node) {
    token_t name_tok = peek();
    if (name_tok.type != TOKEN_IDENTIFIER) {
        error("expected identifier at line %d, found `%s`", name_tok.line,
              peek().lexeme);
        return -1;
    }
    advance(); // consume identifier

    if (!match(TOKEN_LPAREN)) {
        error("expected `(` after function call at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t *call = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!call) {
        perr("parser: failed to allocate memory for `call` node");
        return -1;
    }

    // argument parsing
    int capacity = 4;
    int count = 0;
    ast_node_t **args = malloc(sizeof(ast_node_t *) * capacity);
    if (!args) {
        perr("parser: failed to allocate memory for call arguments");
        free(call->call.name);
        free(call);
        return -1;
    }

    if (!match(TOKEN_RPAREN)) {
        do {
            if (count >= capacity) {
                capacity *= 2;
                ast_node_t **new_args =
                    realloc(args, sizeof(ast_node_t *) * capacity);
                if (!new_args) {
                    perr("parser: failed to reallocate memory for call "
                         "arguments");
                    for (int i = 0; i < count; ++i) {
                        free_ast_node(args[i]);
                    }
                    free(args);
                    free(call->call.name);
                    free(call);
                    return -1;
                }
                args = new_args;
            }

            ast_node_t *arg = NULL;
            if (parse_expr(&arg, false) != 0) {
                error("failed to parse function call argument at line %d",
                      peek().line);
                for (int i = 0; i < count; ++i) {
                    free_ast_node(args[i]);
                }
                free(args);
                free(call->call.name);
                free(call);
                return -1;
            }

            args[count++] = arg;

        } while (match(TOKEN_COMMA));
    }

    if (count != 0) {
        if (!match(TOKEN_RPAREN)) {
            error("expected `)` at line %d, found `%s`", name_tok.line,
                  peek().lexeme);
            return -1;
        }
    }

    call->type = AST_CALL;
    call->call.name = strdup(name_tok.lexeme);
    if (!call->call.name) {
        perr("parser: failed to allocate memory for `call->name` string");
        free(call);
        return -1;
    }
    call->call.args = args;
    call->call.param_count = count;

    *node = call;
    return 0;
}

// if it's not an index node, returns 1
// if it is returns 0
// if it fails returns -1
static int parse_index(ast_node_t **node) {
    token_t name_tok = peek();
    if (name_tok.type != TOKEN_IDENTIFIER) {
        error("expected identifier at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    ast_node_t *array = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!array) {
        perr("parser: failed to allocate memory for `array` node");
        return -1;
    }
    array->type = AST_IDENTIFIER;
    array->identifier.name = strdup(name_tok.lexeme);
    if (!array->identifier.name) {
        perr("parser: failed to allocate memory for `array->identifier.name`");
        free(array);
        return -1;
    }

    // save current position to backtrack if no index found
    int saved_pos = parser.current;

    // consume identifier token
    advance();

    if (!match(TOKEN_LBRACKET)) {
        parser.current = saved_pos;
        free_ast_node(array);
        return 1;
    }

    ast_node_t *index = NULL;
    if (parse_expr(&index, false) != 0) {
        free_ast_node(array);
        return -1;
    }

    if (!match(TOKEN_RBRACKET)) {
        error("expected `]` at line %d, found `%s`", peek().line,
              peek().lexeme);
        free_ast_node(index);
        free_ast_node(array);
        return -1;
    }

    ast_node_t *idx = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!idx) {
        perr("parser: failed to allocate memoryf for `index` node");
        free_ast_node(index);
        free_ast_node(array);
        return -1;
    }
    idx->type = AST_INDEX;
    idx->index.array = array;
    idx->index.index = index;

    *node = idx;
    return 0;
}

static int parse_block(ast_node_t **node);

static int parse_if(ast_node_t **node) {
    if (!match(TOKEN_IF)) {
        error("expected `if` keyword at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    if (!match(TOKEN_LPAREN)) {
        error("expected `(` after `if` keyword at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t *condition = NULL;
    if (parse_expr(&condition, false) != 0) {
        return -1;
    }

    if (!match(TOKEN_RPAREN)) {
        error("expected `)` at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    ast_node_t *then_block = NULL;
    if (parse_block(&then_block) != 0) {
        return -1;
    }

    ast_node_t *if_stmt = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!if_stmt) {
        perr("parser: failed to allocate memory for `if` node");
        return -1;
    }

    ast_node_t *else_block = NULL;

    if (match(TOKEN_ELSE)) {
        if (parse_block(&else_block) != 0) {
            free(if_stmt);
            return -1;
        }
    }

    if_stmt->type = AST_IF;
    if_stmt->if_stmt.condition = condition;
    if_stmt->if_stmt.then_block = then_block;
    if_stmt->if_stmt.else_block = else_block;

    *node = if_stmt;
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

    type_t type = type_void();
    if (match(TOKEN_COLON)) {
        if (parse_type(&type) != 0) {
            return -1;
        }
    }

    if (type.kind == TYPE_PRIMARY && type.data.primary == TYPE_VOID) {
        error(
            "type annotation required for `let` binding at line %d, found `%s`",
            peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t *value = NULL;
    if (match(TOKEN_EQUAL)) {
        if (parse_expr(&value, false) != 0) {
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
        if (parse_expr(&expr, false) != 0) {
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

    ast_node_t *ret = (ast_node_t *)malloc(sizeof(ast_node_t));
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

static int parse_while(ast_node_t **node) {
    if (!match(TOKEN_WHILE)) {
        error("expected `while` keyword at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    if (!match(TOKEN_LPAREN)) {
        error("expected `(` after `while` keyword at line %d, found `%s`",
              peek().line, peek().lexeme);
        return -1;
    }

    ast_node_t *condition = NULL;
    if (parse_expr(&condition, false) != 0) {
        return -1;
    }

    if (!match(TOKEN_RPAREN)) {
        error("expected `)` at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    ast_node_t *body = NULL;
    if (parse_block(&body) != 0) {
        return -1;
    }

    ast_node_t *while_stmt = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!while_stmt) {
        perr("parser: failed to allocate memory for `while` node");
        return -1;
    }
    while_stmt->type = AST_WHILE;
    while_stmt->while_stmt.condition = condition;
    while_stmt->while_stmt.body = body;

    *node = while_stmt;
    return 0;
}

static int parse_assignable(ast_node_t **node) {
    token_t tok = peek();
    if (tok.type != TOKEN_IDENTIFIER) {
        error("expected identifier at line %d, found `%s`", peek().line,
              peek().lexeme);
        return -1;
    }

    // Try parsing index first
    ast_node_t *lhs = NULL;
    int idx_res = parse_index(&lhs);
    if (idx_res == -1)
        return -1;

    // If no index, try simple identifier node
    if (idx_res == 1) {
        // Not an index, just simple identifier
        lhs = (ast_node_t *)malloc(sizeof(ast_node_t));
        lhs->type = AST_IDENTIFIER;
        lhs->identifier.name = strdup(tok.lexeme);
        if (!lhs->identifier.name) {
            perr(
                "parser: failed to allocate memory for `lhs->identifier.name`");
            free(lhs);
            return -1;
        }
        advance();
    }

    // Now expect '='
    if (!match(TOKEN_EQUAL)) {
        // Not assignment, backtrack
        free_ast_node(lhs);
        return 1;
    }

    ast_node_t *rhs = NULL;
    if (parse_expr(&rhs, false) != 0) {
        free_ast_node(lhs);
        return -1;
    }

    if (!match(TOKEN_SEMICOLON)) {
        error("expected `;` at line %d, found `%s`", peek().line,
              peek().lexeme);
        free_ast_node(lhs);
        free_ast_node(rhs);
        return -1;
    }

    ast_node_t *assign = (ast_node_t *)malloc(sizeof(ast_node_t));
    if (!assign) {
        perr("parser: failed to allocate memory for `assign` node");
        free_ast_node(lhs);
        free_ast_node(rhs);
        return -1;
    }

    assign->type = AST_ASSIGN;

    // For assignment nodes, you can add a field for lhs expression:
    // (your current AST_ASSIGN only has a name string, you'll want to
    // generalize it to accept an expression on the LHS, e.g. identifier or
    // index)

    assign->assign.lhs = lhs; // lhs could be identifier or index node
    assign->assign.value = rhs;

    *node = assign;
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
        case TOKEN_IF:
            if (parse_if(&stmt) != 0) {
                return -1;
            }
            break;
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
        case TOKEN_WHILE:
            if (parse_while(&stmt) != 0) {
                return -1;
            }
            break;
        case TOKEN_IDENTIFIER: {
            int ret = parse_assignable(&stmt);
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

static int parse_fn(ast_node_t **node) {
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

            type_t param_type;
            if (parse_type(&param_type) != 0) {
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
                for (int i = 0; i < param_count; ++i) {
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
                for (int i = 0; i < param_count; ++i) {
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
    type_t ret_type;
    if (parse_type(&ret_type) != 0) {
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
        for (int i = 0; i < param_count; ++i) {
            free(params[i].name);
        }
        free(params);
        free_ast_node(body);
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
            if (parse_fn(&decl) != 0) {
                for (int i = 0; i < decl_count; ++i) {
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
            for (int i = 0; i < decl_count; ++i) {
                free_ast_node(decls[i]);
            }
            free(decls);
            return -1;
        }

        ast_node_t **tmp = (ast_node_t **)realloc(decls, sizeof(ast_node_t *) *
                                                             (decl_count + 1));
        if (!tmp) {
            perr("parser: failed to allocate memory for declarations");
            for (int i = 0; i < decl_count; ++i) {
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
        for (int i = 0; i < decl_count; ++i) {
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
        for (int i = 0; i < node->program.decl_count; ++i) {
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
        for (int i = 0; i < node->block.stmt_count; ++i) {
            free_ast_node(node->block.stmt[i]);
        }
        free(node->block.stmt);
        break;

    case AST_ASSIGN:
        free_ast_node(node->assign.lhs);
        free_ast_node(node->assign.value);
        break;

    case AST_UNARY:
        free_ast_node(node->unary.right);
        break;

    case AST_BINARY:
        free_ast_node(node->binary.left);
        free_ast_node(node->binary.right);
        break;

    case AST_CALL:
        for (int i = 0; i < node->call.param_count; ++i) {
            free_ast_node(node->call.args[i]);
        }
        free(node->call.args);
        free(node->call.name);
        break;

    case AST_INDEX:
        free_ast_node(node->index.array);
        free_ast_node(node->index.index);
        break;

    case AST_FUNCTION:
        free(node->func.name);
        for (int i = 0; i < node->func.param_count; ++i) {
            free(node->func.params[i].name);
        }
        free(node->func.params);
        free_ast_node(node->func.body);
        break;

    case AST_IF:
        free_ast_node(node->if_stmt.condition);
        free_ast_node(node->if_stmt.then_block);
        if (node->if_stmt.else_block) {
            free_ast_node(node->if_stmt.else_block);
        }
        break;

    case AST_LET:
        free(node->let.name);
        free_ast_node(node->let.value);
        break;

    case AST_RETURN:
        free_ast_node(node->return_stmt.value);
        break;

    case AST_WHILE:
        free_ast_node(node->while_stmt.condition);
        free_ast_node(node->while_stmt.body);
        break;

    default:
        warning("unsupported node type for freeing");
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
