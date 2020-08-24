(
    (lambda (foldr foldl a b) (foldl '(lambda (c d) (cons c (d))) '() '(a b)))

    '(label foldr (lambda (f z u) (cond
        ((atom u) z)
        ('t (f (car u) (foldr f z (cdr u)))) )))

    '(lambda (f z u) (
        (label outer (lambda (f z u r) (cond
            ((atom u) (
                (label inner (lambda (f1 z1 r1) (cond
                    ((atom r1) z1)
                    ('t (f1 (inner f1 z1 (cdr r1)) (car r1))) )))
                f z r ))
            ('t (outer f z (cdr u) (cons (car u) r))) )))
        f z u '() ))

    '(lambda () 'x)
    '(lambda () 'y)
)
