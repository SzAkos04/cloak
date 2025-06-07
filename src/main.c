#include "cli.h"

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

    return 0;
}
