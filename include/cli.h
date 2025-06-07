#pragma once

#include <stdbool.h>

typedef struct {
    const char *filename;

    bool show_help;
    bool show_version;
} cli_opts_t;

int parse_cli(int argc, char **argv, cli_opts_t *opts);

void help(void);
void version(void);
