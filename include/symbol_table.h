#pragma once

#include <stdbool.h>

#include <llvm-c/Types.h>

typedef struct symbol_s {
    char *name;
    LLVMValueRef value;
    bool is_mutable;
    struct symbol_s *next;
} symbol_t;

typedef struct {
    symbol_t *head;
} symbol_table_t;

int symbol_table_add(symbol_table_t *symtab, const char *name,
                     LLVMValueRef value, bool is_mutable);
symbol_t *symbol_table_lookup(symbol_table_t *symtab, const char *name);

void symbol_table_free(symbol_table_t *symtab);
