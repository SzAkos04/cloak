#pragma once

#include <stddef.h>

// MUST BE FREED
char *read_file(const char *filename, size_t *file_size);
