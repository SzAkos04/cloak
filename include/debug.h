#pragma once

#include <stdbool.h>
#include <stdio.h>

extern bool use_color;
extern bool silent;

#define COLOR(x) (use_color ? x : "")

#define RESET COLOR("\033[;0m")
#define BOLD COLOR("\033[1m")
#define DIM COLOR("\033[2m")
#define UNDERLINE COLOR("\033[4m")
#define INVERT COLOR("\033[7m")
#define STRIKETHROUGH COLOR("\033[9m")

#define BLACK COLOR("\033[30m")
#define RED COLOR("\033[31m")
#define GREEN COLOR("\033[32m")
#define YELLOW COLOR("\033[33m")
#define BLUE COLOR("\033[34m")
#define MAGENTA COLOR("\033[35m")
#define CYAN COLOR("\033[36m")
#define WHITE COLOR("\033[37m")

#define BRIGHT_BLACK COLOR("\033[90m")
#define BRIGHT_RED COLOR("\033[91m")
#define BRIGHT_GREEN COLOR("\033[92m")
#define BRIGHT_YELLOW COLOR("\033[93m")
#define BRIGHT_BLUE COLOR("\033[94m")
#define BRIGHT_MAGENTA COLOR("\033[95m")
#define BRIGHT_CYAN COLOR("\033[96m")
#define BRIGHT_WHITE COLOR("\033[97m")

#ifdef DEBUG
#define error(...)                                                             \
    do {                                                                       \
        fprintf(stderr, "%scloak: %serror%s [%s:%d:%s())]: ", BOLD, RED,       \
                RESET, __FILE__, __LINE__, __func__);                          \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
    } while (0)

#define perr(...)                                                              \
    do {                                                                       \
        fprintf(stderr, "%scloak: %serror%s [%s:%d:%s())]: ", BOLD, RED,       \
                RESET, __FILE__, __LINE__, __func__);                          \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
        perror("  ↳ system error");                                            \
    } while (0)

#define warning(...)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stderr, "%scloak: %swarning%s [%s:%d:%s())]: ", BOLD,      \
                    YELLOW, RESET, __FILE__, __LINE__, __func__);              \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define info(...)                                                              \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stderr, "%scloak: info%s [%s:%d:%s())]: ", BOLD, RESET,    \
                    __FILE__, __LINE__, __func__);                             \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define success(...)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stderr, "%scloak: %ssuccess%s [%s:%d:%s())]: ", BOLD,      \
                    GREEN, RESET, __FILE__, __LINE__, __func__);               \
            fprintf(stdout, "%scloak: %ssuccess%s: ", BOLD, GREEN, RESET);     \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define debug(...)                                                             \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stdout, "%scloak: %sdebug%s [%s:%d: %s())]: ", BOLD, BLUE, \
                    RESET, __FILE__, __LINE__, __func__);                      \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)
#else
#define error(...)                                                             \
    do {                                                                       \
        fprintf(stderr, "%scloak: %serror%s: ", BOLD, RED, RESET);             \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
    } while (0)

#define perr(...)                                                              \
    do {                                                                       \
        fprintf(stderr, "%scloak: %serror%s: ", BOLD, RED, RESET);             \
        fprintf(stderr, __VA_ARGS__);                                          \
        fprintf(stderr, "\n");                                                 \
        perror("  ↳ system error");                                            \
    } while (0)

#define warning(...)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stdout, "%scloak: %swarning%s: ", BOLD, YELLOW, RESET);    \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define info(...)                                                              \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stdout, "%scloak: info%s: ", BOLD, RESET);                 \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define success(...)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            fprintf(stdout, "%scloak: %ssuccess%s: ", BOLD, GREEN, RESET);     \
            fprintf(stdout, __VA_ARGS__);                                      \
            fprintf(stdout, "\n");                                             \
        }                                                                      \
    } while (0)

#define debug(...)                                                             \
    do {                                                                       \
    } while (0)
#endif

#define MAYBE_UNUSED __attribute__((unused))
