#include "fs_utils.h"

#include "debug.h"

#include <stdio.h>
#include <stdlib.h>

char *read_file(const char *filename, size_t *file_size) {
    *file_size = 0;

    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        perr("failed to open `%s`", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = (char *)malloc(*file_size + 1);
    if (buffer == NULL) {
        perr("failed to allocate memory to `%s` buffer", filename);
        fclose(file);
        return NULL;
    }

    size_t bytesRead = fread(buffer, 1, *file_size, file);
    if (bytesRead != *file_size) {
        perr("failed to read `%s`", filename);
        free(buffer);
        fclose(file);
        return NULL;
    }

    fclose(file);

    buffer[*file_size] = '\0';

    return buffer;
}
