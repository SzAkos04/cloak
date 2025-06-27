#pragma once

#include <cstring>
#include <iostream>

extern bool useColor;
extern bool silent;

#define COLOR(x) (useColor ? x : "")

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

#define error(msg)                                                             \
    do {                                                                       \
        std::cerr << BOLD << "cloak: " << RED << "error" << RESET << ": "      \
                  << msg << std::endl;                                         \
    } while (0)

#define perr(msg)                                                              \
    do {                                                                       \
        std::cerr << BOLD << "cloak: " << RED << "error" << RESET << ": "      \
                  << msg << std::endl;                                         \
        std::cerr << "  \u21B3 system error: " << strerror(errno)              \
                  << std::endl;                                                \
    } while (0)

#define warning(msg)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            std::cout << BOLD << "cloak: " << YELLOW << "warning" << RESET     \
                      << ": " << msg << std::endl;                             \
        }                                                                      \
    } while (0)

#define info(msg)                                                              \
    do {                                                                       \
        if (!silent) {                                                         \
            std::cout << BOLD << "cloak: info" << RESET << ": " << msg         \
                      << std::endl;                                            \
        }                                                                      \
    } while (0)

#ifdef DEBUG
#define debug(msg)                                                             \
    do {                                                                       \
        if (!silent) {                                                         \
            std::cout << BOLD << "cloak: " << BLUE << "[" << __FILE__ << ":"   \
                      << __LINE__ << "]" << RESET << ": " << msg << std::endl; \
        }                                                                      \
    } while (0)
#else
#define debug(msg)                                                             \
    do {                                                                       \
    } while (0)
#endif

#define success(msg)                                                           \
    do {                                                                       \
        if (!silent) {                                                         \
            std::cout << BOLD << "cloak: " << GREEN << "success" << RESET      \
                      << ": " << msg << std::endl;                             \
        }                                                                      \
    } while (0)

#define MAYBE_UNUSED __attribute__((unused))
