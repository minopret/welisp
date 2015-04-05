(   (lambda (assoc caar cadar caddr cadr cdar evcon evlis null pairlis apply eval)
        (caar '(( ((()) ()) )) )
        )

    '(label assoc (lambda (x a) (cond
        ((eq (caar a) x) (car a))
        ('t (assoc x (cdr a))) ))) ; assoc
    '(lambda (x) (car (car x))) ; caar
    '(lambda (x) (car (cdr (car x)))) ; cadar
    '(lambda (x) (car (cdr (cdr x)))) ; caddr
    '(lambda (x) (car (cdr x))) ; cadr
    '(lambda (x) (cdr (car x))) ; cdar
    '(label evcon (lambda (c a) (cond
        ((eval (caar c) a) (eval (cadar c) a))
        ('t (evcon (cdr c) a)) ))) ; evcon
    '(label evlis (lambda (m a) (cond
        ((null m) '())
        ('t (cons (eval (car m) a) (evlis (cdr m) a))) ))) ; evlis
    '(lambda (x) (eq x '())) ; null
    '(label pairlis (lambda (x y a) (cond
        ((null x) a)
        ('t (cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a))) ))) ; pairlis

    '(label apply (lambda (fn x a) (cond
        ((atom fn) (cond
            ((eq fn 'car) (caar x))
            ((eq fn 'cdr) (cdar x))
            ((eq fn 'cons) (cons (car x) (cadr x)))
            ((eq fn 'atom) (atom (car x)))
            ((eq fn 'eq) (eq (car x) (cadr x)))
            ('t (apply (eval fn a) x a)) ))
        ((eq (car fn) 'lambda) (eval (caddr fn) (pairlis (cadr fn) x a)))
        ((eq (car fn) 'label) (apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a))) ))) ; apply

    '(lambda (e a) (cond
        ((atom e) (cdr (assoc e a)))
        ((atom (car e)) (cond
            ((eq (car e) 'quote) (cadr e))
            ((eq (car e) 'cond) (evcon (cdr e) a))
            ('t (apply (car e) (evlis (cdr e) a) a)) )) )) ; eval
)