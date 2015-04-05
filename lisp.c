/*
 * Aaron Mansheim
 * 2015-03-24
 */

#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lisp.h"



alloc_t *read(FILE *f);
alloc_t *eval(alloc_t *e, alloc_t *a);
void display(alloc_t *a);

jmp_buf fatal;

typedef enum {
    ERROR_CAR,
    ERROR_CDR,
    ERROR_CONS,
    ERROR_RIGHT_PAREN,
    ERROR_PUSHBACK,
    ERROR_SYMBOL_NAME,
    ERROR_STEP
} error_t;


#define SYMBOL_DATA(NAME) \
alloc_t NAME ## _storage = {SYMBOL, { #NAME }}; \
alloc_t * NAME ## _symbol = & NAME ## _storage;

SYMBOL_DATA(atom)
SYMBOL_DATA(car)
SYMBOL_DATA(cdr)
SYMBOL_DATA(cond)
SYMBOL_DATA(cons)
SYMBOL_DATA(eq)
SYMBOL_DATA(label)
SYMBOL_DATA(lambda)
SYMBOL_DATA(quote)
SYMBOL_DATA(t)

alloc_t nil_storage = {CELL, {0}};
alloc_t *nil = &nil_storage;



alloc_t *new_alloc() {
    return (alloc_t *) malloc(sizeof(alloc_t));
}

void free_alloc(alloc_t *x) {
    free(x);
}



alloc_t *atom(alloc_t *a) {
    return (a->type == SYMBOL || a == nil) ? t_symbol : nil;
}

alloc_t *car(alloc_t *a) {
    if (a->type != CELL) {
        longjmp(fatal, ERROR_CAR);
    }
    return a->value.cell.car;
}

alloc_t *cdr(alloc_t *a) {
    if (a->type != CELL) {
        longjmp(fatal, ERROR_CDR);
    }
    return a->value.cell.cdr;
}

alloc_t *cons(alloc_t *a, alloc_t *b) {
    alloc_t *c;

    c = new_alloc();
    if (!c) {
        longjmp(fatal, ERROR_CONS);
    }
    c->type = CELL;
    c->value.cell.car = a;
    c->value.cell.cdr = b;

    return c;
}

/*
alloc_t *eq(alloc_t *a, alloc_t *b) {
    return (a == b) ? t_symbol : nil;
}
*/
alloc_t *eq(alloc_t *a, alloc_t *b) {
    if (a == b ||
            (a->type == SYMBOL && b->type == SYMBOL
            && strcmp(a->value.symbol.name, b->value.symbol.name) == 0)) {
        return t_symbol;
    } else {
        return nil;
    }
}

/* x is an symbol;
 * a is the relevant association list i.e. environment
 */
alloc_t *assoc(alloc_t *x, alloc_t *a) {

    /* Tail call */
    while (a != nil && eq(car(car(a)), x) == nil) {
        a = cdr(a);
    }

    return (a == nil) ? nil : car(a);
}

/* x, y, and a are lists;
 * y is at least as long as x
 */
alloc_t *pairlis(alloc_t *x, alloc_t *y, alloc_t *a) {
    
    /* Tail call modulo cons */
    alloc_t *r = nil;
    alloc_t *r_old;
    
    while (x != nil) {
        printf("pairing: ");
        display(r);
        printf("\n");
        r = cons(cons(car(x), car(y)), r);
        x = cdr(x);
        y = cdr(y);
    }
    printf("paired : ");
    display(r);
    printf("\n");
    
    while (r != nil) {
        printf("reversing: ");
        display(a);
        printf("\n");
        a = cons(car(r), a);
        r_old = r;
        r = cdr(r);
        free_alloc(r_old);
    }
    
    printf("pairlis ==> ");
    display(a);
    printf("\n");
    
    return a;
}

alloc_t *evlis(alloc_t *m, alloc_t *a) {
    alloc_t *e;
    
    /* Tail call modulo cons */
    alloc_t *r = nil;
    alloc_t *r_old;

    while (m != nil) {
        e = eval(car(m), a);
        r = cons(e, r);
        m = cdr(m);
    }
    
    while (r != nil) {
        m = cons(car(r), m);
        r_old = r;
        r = cdr(r);
        free_alloc(r_old);
    }
    
    return m;
}



typedef enum {
    EVAL,
    APPLY,
    EVCON
} eval_op_t;

#define MAX_STEP (65536)

alloc_t *eval(alloc_t *e, alloc_t *a) {
    eval_op_t eval_op = EVAL;
    alloc_t *f;
    int i;
    
    /* These three functions are mutually recursive via tail-call.
     * We could rewrite to "simplify" the control flow but it
     * wouldn't make any real difference. This way it reflects the Lisp code.
     */
    for (i = 0; i < MAX_STEP; i++) {
    
        printf("%d. ", i + 1);
        if (eval_op == APPLY) {
            display(f);
            printf("\n@\n");
        }
    
        display(e);
        printf("\n:\n");
        display(a);
        printf("\n\n");
    
        printf("\n===> %d\n", eval_op);
        
        switch (eval_op) {
        case EVAL:
            if (atom(e) != nil) {
                return cdr(assoc(e, a));
            } else if (atom(car(e)) != nil) {
                if (eq(car(e), quote_symbol) != nil) {
                    return car(cdr(e));
                } else if (eq(car(e), cond_symbol) != nil) {
                    eval_op = EVCON;
                    e = cdr(e);
                } else {
                    eval_op = APPLY;
                    f = car(e);
                    e = evlis(cdr(e), a);
                }
            } else {
                eval_op = APPLY;
                f = car(e);
                e = evlis(cdr(e), a);
            }
            break;
        case APPLY:
            if (atom(f) != nil) {
                if (eq(f, car_symbol) != nil) {
                    return car(car(e));
                } else if (eq(f, cdr_symbol) != nil) {
                    return cdr(car(e));
                } else if (eq(f, cons_symbol) != nil) {
                    return cons(car(e), car(cdr(e)));
                } else if (eq(f, atom_symbol) != nil) {
                    return atom(car(e));
                } else if (eq(f, eq_symbol) != nil) {
                    return eq(car(e), car(cdr(e)));
                } else {
                    /* eval_op = APPLY; */
                    f = eval(f, a);
                }
            } else if (eq(car(f), lambda_symbol) != nil) {
                eval_op = EVAL;
                a = pairlis(car(cdr(f)), e, a);
                e = car(cdr(cdr(f)));
            } else { /* eq(car(f), label_symbol) != nil */
                /* eval_op = APPLY */
                f = car(cdr(cdr(f)));
                a = cons(cons(car(cdr(f)), car(cdr(cdr(f)))), a);
            }
            break;
        default: /* EVCON */
            if (eval(car(car(e)), a) != nil) {
                eval_op = EVAL;
                e = car(cdr(car(e)));
            } else {
                /* eval_op = EVCON */
                e = cdr(e);
            }
            break;
        }
    }

    longjmp(fatal, ERROR_STEP);
}

/*
(evlis (label evlis (lambda (m a) (cond
    ((null m) (quote ()))
    ((quote t) (cons (eval (car m) a) (evlis (cdr m) a))) ))))

    may call eval NOT via tail-call
    may tail-call self modulo cons
    
    See alternate implementation note on evcon for another (dismissed) thought.

(evcon (label evcon (lambda (c a) (cond
    ((eval (caar c) a) (eval (cadar c) a))
    ((quote t) (evcon (cdr c) a)) ))))

    may call eval NOT via tail-call
    and THEN will either tail-call eval, or tail-call self
    
    alternate implementation:
    push a pair (a continuation):
        (action expression for eval if condition evaluates non-nil,
         action expression for self if condition evaluates nil);
    THEN tail-call eval, and when eval would otherwise be ready to return,
        pop and proceed with the next continuation.
    Eh, this seems like overdoing it unless we want to make the whole interpreter run
    on continuations. Never mind. Just use the first design, which uses the call stack.
*/



void display_more(alloc_t *d, bool is_first) {
    int MAX_LENGTH = 256;
    int i = MAX_LENGTH;

    do {
        if (!is_first) {
            printf(" ");
        }
        display(d->value.cell.car);
    
        d = d->value.cell.cdr;
    
        if (d == NULL) {
            printf(" . NULL!)");
            break;
        } else if (d->type == SYMBOL) {
            printf(" . %s)", d->value.symbol.name);
            break;
        } else if (d == nil) {
            printf(")");
            break;
        }
        is_first = false;
    } while (i-- > 0);
    
    if (i <= 0) {
        printf(" ...)");
    }
}

void display(alloc_t *a) {
    if (a == NULL) {
        printf("NULL!");
    } else if (a == nil) {
        printf("()");
    } else if (a->type == SYMBOL) {
        printf("%s", a->value.symbol.name);
    } else {
        printf("(");
        display_more(a, true);
    }
}



#define BEGIN_LINE_COMMENT ';'
#define END_LINE '\n'
#define WHITESPACE " \t\n"
#define DELIMITERS "'();"

char next_token_char(FILE *f) {
    char c;
    
    for (c = fgetc(f); c != EOF; c = fgetc(f)) {
        if (c == BEGIN_LINE_COMMENT) {
            for (c = fgetc(f); c != EOF && c != END_LINE; c = fgetc(f)) {
                ;
            }
        } else if (!strchr(WHITESPACE, c)) {
            break;
        }
    }
    
    return c;
}

alloc_t *read_quote(FILE *f) {
    alloc_t *x = read(f);
    
    return cons(quote_symbol, cons(x, nil));
}

alloc_t *read_list(FILE *f) {
    alloc_t *x, *r_old, *r = nil;
    char c;
    
    for (c = next_token_char(f); c != ')'; c = next_token_char(f)) {
        c = ungetc(c, f);
        if (c == EOF) {
            longjmp(fatal, ERROR_PUSHBACK);
            /* Occurs if we failed to push back a character for re-reading;
             * but also if we reached end-of-file without closing all parentheses
             */
        }
        x = read(f);
        r = cons(x, r);
    }
    
    x = nil;
    while (r != nil) {
        x = cons(car(r), x);
        r_old = r;
        r = cdr(r);
        free_alloc(r_old);
    }
    
    return x;
}

#define SEGMENT_SIZE (256)
#define MAX_SEGMENT (SIZE_MAX / (2 * SEGMENT_SIZE))

alloc_t *read_symbol(FILE *f) {
    alloc_t *x;
    char c;
    ptrdiff_t position = 0;
    size_t segment = 0;
    char *s = (char *) malloc(sizeof(char) * (segment + 1) * SEGMENT_SIZE);
    bool in_symbol = true;

    if (!s) {
        longjmp(fatal, ERROR_SYMBOL_NAME);
    }
    
    x = new_alloc();
    if (!x) {
        longjmp(fatal, ERROR_SYMBOL_NAME);
    }
    x->type = SYMBOL;
    
    while (true) {
        for (c = fgetc(f); position < SEGMENT_SIZE - 1 && c != EOF; position++, c = fgetc(f)) {
            if (strchr(WHITESPACE DELIMITERS, c)) {
                c = ungetc(c, f);
                if (c == EOF && ferror(f)) {
                    longjmp(fatal, ERROR_PUSHBACK);
                }
                in_symbol = false;
                break;
            }
            s[segment * SEGMENT_SIZE + position] = c;
        }
        
        if (SEGMENT_SIZE - 1 <= position) {
            /* Note that this can happen at EOF as well as before EOF */
            position = -1;
            segment++;
            if (MAX_SEGMENT <= segment) {
                longjmp(fatal, ERROR_SYMBOL_NAME);
            }
            s = (char *) realloc(s, sizeof(char) * (segment + 1) * SEGMENT_SIZE);
            if (!s) {
                longjmp(fatal, ERROR_SYMBOL_NAME);
            }
        }
        
        if (c == EOF || !in_symbol) {
            break;
        }
        s[segment * SEGMENT_SIZE + position] = c;
        position = 0;   
    }
    
    s[segment * SEGMENT_SIZE + position] = '\0';
    
    x->value.symbol.name = s;
    return x;
}

alloc_t *read(FILE *f) {
    alloc_t *x;
    char c;

    c = next_token_char(f);
    if (c == '\'') {
        x = read_quote(f);
    } else if (c == '(') {
        x = read_list(f);
    } else if (c == ')') {
        longjmp(fatal, ERROR_RIGHT_PAREN);
    } else if (c != EOF) {
        c = ungetc(c, f);
        if (c == EOF) {
            longjmp(fatal, ERROR_PUSHBACK);
        }
        x = read_symbol(f);
    }
    return x;
}



int main(void) {
    alloc_t *a, a_storage, *b, b_storage;
    alloc_t *c, *d;
    int result;
    
    nil->value.cell.cdr = nil;
    
    result = setjmp(fatal);
    if (result) {
        switch(result) {
        case ERROR_CAR:
            printf("Fatal: Only a cell can have a car!\n");
            break;
        case ERROR_CDR:
            printf("Fatal: Only a cell can have a cdr!\n");
            break;
        case ERROR_CONS:
            printf("Fatal: Failed to cons!\n");
            break;
        case ERROR_RIGHT_PAREN:
            printf("Fatal: A right parenthesis does not"
                   " match any preceding left parenthesis!\n");
            break;
        case ERROR_PUSHBACK:
            printf("Fatal: Failed to push a character back for re-reading!\n");
            break;
        case ERROR_SYMBOL_NAME:
            printf("Fatal: Failed to allocate space to store a symbol name!\n");
            break;
        case ERROR_STEP:
            printf("Fatal: Ran the maximum allowed steps of eval/apply/evcon!\n");
            break;
        default:
            printf("Fatal: Unknown condition?!\n");
            break;
        }
        return result;
    }


    /*
    a = &a_storage;
    b = &b_storage;

    a->type = CELL;
    a->value.cell.car = quote_symbol;
    a->value.cell.cdr = b;

    b->type = CELL;
    b->value.cell.car = t_symbol;
    b->value.cell.cdr = nil;
    
    
    
    display(a);
    printf("\n");
    
    display(cons(cons_symbol, cons(car_symbol, nil)));
    printf("\n");
    
    c = cons(cons_symbol, cons(car_symbol, nil));
    d = pairlis(a, c, nil);
    
    display(d);
    printf("\n");

    display(eval(a, d));
    printf("\n");
    */
    
    FILE *in = fopen("test.lsp", "r");
    display(eval(read(in), nil));
    fclose(in);
    printf("\n");

    return 0;
}
