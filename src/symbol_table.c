#include "symbol_table.h"

#include "debug.h"

#include <llvm-c/Types.h>
#include <stdlib.h>
#include <string.h>

int symbol_table_add(symbol_table_t *symtab, const char *name, LLVMTypeRef type,
                     LLVMValueRef value, bool is_mutable) {
    // Check for duplicates
    for (symbol_t *cur = symtab->head; cur != NULL; cur = cur->next) {
        if (strcmp(cur->name, name) == 0) {
            error("duplicate symbol `%s`", name);
            return -1;
        }
    }

    symbol_t *sym = (symbol_t *)malloc(sizeof(symbol_t));
    if (!sym) {
        perr("symbol_table: failed to allocate memory for `sym` symbol");
        return -1;
    }
    sym->name = strdup(name);
    if (!sym->name) {
        free(sym);
        perr("symbol_table: failed to allocate memory for `sym->name` string");
        return -1;
    }
    sym->type = type;
    sym->value = value;
    sym->is_mutable = is_mutable;
    sym->next = symtab->head;
    symtab->head = sym;

    return 0;
}

symbol_t *symbol_table_lookup(symbol_table_t *symtab, const char *name) {
    for (symbol_t *cur = symtab->head; cur != NULL; cur = cur->next) {
        if (strcmp(cur->name, name) == 0)
            return cur;
    }
    return NULL;
}

void symbol_table_free(symbol_table_t *symtab) {
    symbol_t *cur = symtab->head;
    while (cur) {
        symbol_t *next = cur->next;
        free(cur->name);
        free(cur);
        cur = next;
    }
    symtab->head = NULL;
}
