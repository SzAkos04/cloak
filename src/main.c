#include "ast.h"
#include "cli.h"
#include "codegen.h"
#include "debug.h"
#include "fs_utils.h"
#include "lexer.h"
#include "llvm_codegen.h"
#include "parser.h"
#include "token.h"

#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

int main(int argc, char **argv) {
    cli_opts_t opts;
    if (parse_cli(argc, argv, &opts) != 0) {
        return EXIT_FAILURE;
    }

    if (opts.show_help) {
        help();
        return EXIT_SUCCESS;
    }
    if (opts.show_version) {
        version();
        return EXIT_SUCCESS;
    }

    size_t len;
    char *src = read_file(opts.filename, &len);
    if (!src) {
        return EXIT_FAILURE;
    }
    lexer_init(src);

    token_t *tokens;
    int token_count = lex(&tokens);
    if (token_count < 0) {
        free(src);
        return EXIT_FAILURE;
    }
    free(src);

#ifdef DEBUG
    for (int i = 0; tokens[i].type != TOKEN_EOF; ++i) {
        debug_token(tokens[i]);
    }
#endif

    parser_init(tokens, token_count);
    ast_t *ast;
    if (parse_ast(&ast) != 0) {
        return EXIT_FAILURE;
    }

#ifdef DEBUG
    debug_ast(ast);
#endif

    LLVMContextRef context;
    LLVMModuleRef module;
    if (gen_IR(ast, &context, &module) != 0) {
        return -1;
    }

    if (gen_machine_code(module, opts.outfile) != 0) {
        return -1;
    }

    LLVMDisposeModule(module);
    LLVMContextDispose(context);
    free_tokens(tokens, token_count);
    free_ast(ast);

    return EXIT_SUCCESS;
}
