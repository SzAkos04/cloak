#include "cli.h"
#include "fs_utils.h"
#include "lexer.h"
#include "token.h"

#include <stdlib.h>

int main(int argc, char **argv) {
    cli_opts_t opts;
    if (parse_cli(argc, argv, &opts) != 0) {
        return -1;
    }

    if (opts.show_help) {
        help();
        return 0;
    }
    if (opts.show_version) {
        version();
        return 0;
    }

    size_t len;
    char *src = read_file(opts.filename, &len);
    if (!src) {
        return -1;
    }
    lexer_init(src);

    token_t *tokens;
    int token_count = lex(&tokens);
    if (token_count < 0) {
        free(src);
        return -1;
    }
    free(src);

    for (int i = 0; tokens[i].type != TOKEN_EOF; ++i) {
        debug_token(tokens[i]);
    }

    free_tokens(tokens, token_count);

    return 0;
}
