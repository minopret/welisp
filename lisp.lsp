(assoc (label assoc (lambda (x a) (cond
    ((eq (caar a) x) (car a))
    ('t (assoc x (cdr a))) ))))
(caar (lambda (x) (car (car x))))
(cadar (lambda (x) (car (cdr (car x)))))
(caddr (lambda (x) (car (cdr (cdr x)))))
(cadr (lambda (x) (car (cdr x))))
(cdar (lambda (x) (cdr (car x))))
(evcon (label evcon (lambda (c a) (cond
    ((eval (caar c) a) (eval (cadar c) a))
    ('t (evcon (cdr c) a)) ))))
(evlis (label evlis (lambda (m a) (cond
    ((null m) '())
    ('t (cons (eval (car m) a) (evlis (cdr m) a))) ))))
(null (lambda (x) (eq x '())))
(pairlis (label pairlis (lambda (x y a) (cond
    ((null x) a)
    ('t (cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a))) ))))

(apply (label apply (lambda (fn x a) (cond
    ((atom fn) (cond
        ((eq fn 'car) (caar x))
        ((eq fn 'cdr) (cdar x))
        ((eq fn 'cons) (cons (car x) (cadr x)))
        ((eq fn 'atom) (atom (car x)))
        ((eq fn 'eq) (eq (car x) (cadr x)))
        ('t (apply (eval fn a) x a)) ))
    ((eq (car fn) 'lambda) (eval (caddr fn) (pairlis (cadr fn) x a)))
    ((eq (car fn) 'label) (apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a))) ))))

(eval (lambda (e a) (cond
    ((atom e) (cdr (assoc e a)))
    ((atom (car e)) (cond
        ((eq (car e) 'quote) (cadr e))
        ((eq (car e) 'cond) (evcon (cdr e) a))
        ('t (apply (car e) (evlis (cdr e) a) a)) )) )))