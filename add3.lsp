(   (lambda (ddcr32 decr32 incr32 noop32 iicr32 add32 cadr null add3r
             rappend reverse normalize3 add3)

        (add3 '(+ - 0 -) '(+ + - +)) ) ; 17 + 34 = 51 = (+ - 0 - 0)

    '(lambda (x) (cond
        ((eq x '0) '(- +))
        ((eq x '-) '(- 0))
        ('t '(0 -)) )) ; ddcr32

    '(lambda (x) (cond
        ((eq x '0) '(0 -))
        ((eq x '-) '(- +))
        ('t '(0 0)) )) ; decr32

    '(lambda (x) (cond
        ((eq x '0) '(0 +))
        ((eq x '-) '(0 0))
        ('t '(+ -)) )) ; incr32

    '(lambda (x) (cons '0 (cons x '()))) ; noop32

    '(lambda (x) (cond
        ((eq x '0) '(+ -))
        ((eq x '-) '(0 +))
        ('t '(+ 0)) )) ; iicr32

    '(lambda (x y z) (cond
        ((eq x '0) (cond
            ((eq y '0) (noop32 z))
            ((eq y '-) (decr32 z))
            ('t (incr32 z)) ))
        ((eq x '-) (cond
            ((eq y '0) (decr32 z))
            ((eq y '-) (ddcr32 z))
            ('t (noop32 z)) ))
        ((eq x '+) (cond
            ((eq y '0) (incr32 z))
            ((eq y '-) (noop32 z))
            ('t (iicr32 z)) )) )) ; add32

    '(lambda (x) (car (cdr x))) ; cadr

    '(lambda (x) (eq x '())) ; null

    '(label add3r (lambda (x y c r) (cond
        ((null y) (cond
            ((null x) (cond
                ((eq c '0) r)
                ('t (add3r '(0) '(0) c r)) ))
            ('t (add3r x '(0) c r)) ))
        ((null x) (add3r '(0) y c r))
        ('t (   (lambda (pair) (add3r (cdr x) (cdr y) (car pair) (cons (cadr pair) r)))
                (add32 (car x) (car y) c) )) ))) ; add3r

    '(label rappend (lambda (x y) (cond
        ((null x) y)
        ('t (rappend (cdr x) (cons (car x) y))) ))) ; rappend

    '(lambda (x) (rappend x '())) ; reverse

    '(label normalize3 (lambda (x) (cond
        ((eq (car x) '0) (normalize3 (cdr x)))
        ('t x) ))) ; normalize3

    '(lambda (x y) (normalize3 (add3r (reverse x) (reverse y) '0 '()))) ) ; add3