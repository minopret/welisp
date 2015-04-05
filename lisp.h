/*
 * Aaron Mansheim
 * 2015-03-24
 */

struct alloc_t;

typedef struct {
    struct alloc_t *car;
    struct alloc_t *cdr;
} cell_t;

typedef struct {
    char *name;
} symbol_t;

typedef union {
    symbol_t symbol;
    cell_t cell;
} value_t;

typedef enum {
    SYMBOL,
    CELL
} type_t;

struct alloc_t {
    type_t type;
    value_t value;
};

typedef struct alloc_t alloc_t;