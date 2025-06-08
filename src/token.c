#include "debug.h"
#include "token.h"

void debug_token(token_t token) {
    info("{ type: %s; lexeme: \"%s\"; len: %d; line: %d }",
         token_type_to_str(token.type), token.lexeme, token.len, token.line);
}
