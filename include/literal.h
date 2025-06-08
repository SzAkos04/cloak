#pragma once

#include <stdbool.h>

typedef enum {
    LITERAL_NUMBER,
    LITERAL_STRING,
    LITERAL_BOOL,
} literal_type_t;

typedef struct {
    literal_type_t kind;
    union {
        double number;
        char *string;
        bool boolean;
    };
} literal_t;
