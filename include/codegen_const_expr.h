#pragma once

#include "ast.h"

int evaluate_constant_expr(ast_node_t *expr, int *out_value);
