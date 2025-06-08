#include "cli.h"
#include "debug.h"
#include <stdio.h>
#include <string.h>

bool use_color = true;
bool silent = false;

static int opts_default(cli_opts_t *opts) {
    *opts = (cli_opts_t){
        .filename = NULL,
        .outfile = NULL,
        .show_help = false,
        .show_version = false,
    };

    return 0;
}

static int parse_long_arg(const char *arg, int *i, int argc, char **argv,
                          cli_opts_t *opts) {
    if (strcmp(arg, "--help") == 0) {
        opts->show_help = true;
    } else if (strcmp(arg, "--version") == 0) {
        opts->show_version = true;
    } else if (strcmp(arg, "--color") == 0) {
        use_color = true;
    } else if (strcmp(arg, "--no-color") == 0) {
        use_color = false;
    } else if (strcmp(arg, "--silent") == 0) {
        silent = true;
    } else if (strcmp(arg, "--no-silent") == 0) {
        silent = false;
    } else if (strcmp(arg, "--outfile") == 0) {
        if (*i + 1 >= argc) {
            error("--outfile requires a filename");
            return -1;
        }
        opts->outfile = argv[++(*i)];
    } else {
        error("unknown argument `%s`, for more info run `cloak --help`", arg);
        return -1;
    }

    return 0;
}

static int parse_short_arg(const char *arg, int *i, int argc, char **argv,
                           cli_opts_t *opts) {
    int len = strlen(arg);
    for (int j = 1; j < len; ++j) {
        switch (arg[j]) {
        case 'o':
            // if -o is last char in this arg, output filename is next argv
            if (j == len - 1) {
                if (*i + 1 >= argc) {
                    error("-o requires a filename");
                    return -1;
                }
                opts->outfile = argv[++(*i)];
                return 0;
            } else {
                // -o with filename immediately after e.g. -omyfile
                opts->outfile = &arg[j + 1];
                return 0;
            }
            break;
        case 'h':
            opts->show_help = true;
            break;
        case 'v':
            opts->show_version = true;
            break;
        default:
            error("unknown argument `-%c`, for more info run `cloak --help`",
                  arg[j]);
            return -1;
        }
    }

    return 0;
}

static bool has_extension(const char *filename, const char *ext) {
    size_t len_filename = strlen(filename);
    size_t len_ext = strlen(ext);

    if (len_filename < len_ext) {
        return false;
    }

    return strcmp(filename + (len_filename - len_ext), ext) == 0;
}

int parse_cli(int argc, char **argv, cli_opts_t *opts) {
    if (opts_default(opts) != 0) {
        return -1;
    }

    if (argc < 2) {
        error("incorrect usage, for help run `cloak --help`");
        return -1;
    }

    for (int i = 1; i < argc; ++i) {
        const char *arg = argv[i];
        if (arg[0] == '-') { // parse args
            if (strlen(arg) < 2) {
                error("unknown argument `%s`, for more info run `cloak --help`",
                      arg);
                return -1;
            }

            if (arg[1] == '-') { // parse long args
                if (parse_long_arg(arg, &i, argc, argv, opts) != 0) {
                    return -1;
                }
            } else { // parse short args
                if (parse_short_arg(arg, &i, argc, argv, opts) != 0) {
                    return -1;
                }
            }
        } else { // parse filename
            if (opts->filename) {
                error(
                    "multiple input files provided, only one file is allowed");
                return -1;
            }

            if (!has_extension(arg, ".ck")) {
                error("input file `%s` does not have the required `.ck` "
                      "extension",
                      arg);
                return -1;
            }

            opts->filename = arg;
        }
    }

    if (!opts->filename && !opts->show_help && !opts->show_version) {
        error("incorrect usage, for help run `cloak --help`");
        return -1;
    }
    if (!opts->outfile) {
        opts->outfile = "./a.o";
    }

    return 0;
}

void help(void) {
    printf("cloak help\n");
    return;
}

void version(void) {
    printf("cloak version\n");
    return;
}
