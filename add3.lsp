; Finally, let's have an s-expression |e| that we want to evaluate,
; plus an s-expression |a| containing any supporting definitions for |e|
; appended by the preload environment above.
; Do the equivalent of invoking:
;   (eval e a)

(eval

'(add3 '(+ - 0 -) '(+ + - +))

'(   (ddcr32 (lambda (x) (cond
        ((eq x '0) '(- +))
        ((eq x '-) '(- 0))
        ('t '(0 -)) )))

    (decr32 (lambda (x) (cond
        ((eq x '0) '(0 -))
        ((eq x '-) '(- +))
        ('t '(0 0)) )))

    (incr32 (lambda (x) (cond
        ((eq x '0) '(0 +))
        ((eq x '-) '(0 0))
        ('t '(+ -)) )))

    (noop32 (lambda (x) (cons '0 (cons x '()))))

    (iicr32 (lambda (x) (cond
        ((eq x '0) '(+ 0))
        ((eq x '-) '(0 +))
        ('t '(+ 0)) )))

    (addop3 (lambda (x y) (cond
        ((eq x '0) (cond
            ((eq y '0) noop32)
            ((eq y '-) decr32)
            ('t incr32) ))
        ((eq x '-) (cond
            ((eq y '0) decr32)
            ((eq y '-) ddcr32)
            ('t noop32) ))
        ((eq x '+) (cond
            ((eq y '0) incr32)
            ((eq y '-) noop32)
            ('t iicr32) )) )))

    (add32 (lambda (x y z) ((addop3 x y) z)))

    (cadr (lambda (x) (car (cdr x))))

    (null (lambda (x) (eq x '())))

    (add3r (label add3r (x y c r) (cond
        ((null y) (cond
            ((null x) (cond
                ((eq c '0) r)
                ('t (add3r '(0) '(0) c r)) ))
            ('t (add3r x '(0) c r)) ))
        ((null x) (add3r '(0) y c r))
        ('t (   (lambda (pair) (add3r (cdr x) (cdr y) (car pair) (cons (cadr pair) r)))
                (add32 (car x) (car y) c) )) )))

    (rappend (label rappend (lambda (x y) (cond
        ((null x) y)
        ('t (rappend (cdr x) (cons (car x) y))) ))))

    (reverse (lambda (x) (rappend x '())))

    (normalize3 (label normalize3 (lambda (x) (cond
        ((eq (car x) '0) (normalize3 (cdr x)))
        ('t x) ))))

    (add3 (lambda (x y) (normalize3 (add3r (reverse x) (reverse y) '0 '())))) ))