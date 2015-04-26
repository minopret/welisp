; An expression x may be a variable name v, that is, an identifier;
; a lambda expression ('Lambda v x), where v is a variable name and
; x is an expression;
; or a pair of expressions (x y) that represents applying the first
; to the second.
;
; An environment is a list of pairs.
;
; Each pair in an environment consists of a variable followed by a closure,
; where a closure is either the empty list, or else is a nonempty list,
; the first element of is an expression, and
; the rest of which is an environment.
;
; The function lfirst takes a quoted expression and returns a state.
; The function lnext takes a state and returns a state, so that
; starting with output of lfirst, we can compute many iterations of lnext.
;
; When a state is a list and its first element is "Result",
; then that state is a result state, that is, a fixed point of lnext.
; If the result state is also the output of some number of iterations
; of lnext upon the output of lfirst, then the state's second
; element is a closure representing the computed value of the expression
; that was input to lfirst.

(
    (lambda (first second third fourth fifth rest null and    lapply lassoc leval lfirst lnext)
        ;(lfirst '((Lambda x x) (Lambda s s)))
        ; ; 1
        ;(lnext '(Apply (EvalResult ((Lambda x x) (Lambda s s)) () Done)))
        ; ; 2
        ;(lnext '(Apply (CloseFunc (Lambda x x) (Lambda s s) () Done)))
        ; ; 6
        ;(lnext '(Apply (EvalArg ((Lambda x x)) (Lambda s s) () Done)))
        ; ; 8
        ;(lnext '(Apply (CloAtArg ((Lambda x x)) ((Lambda s s)) () Done)))
        ; ; 9
        ;(lnext '(Apply (EvalResult x ((x ((Lambda s s)))) Done)))
        ;(lnext '(Result ((Lambda s s))))   ; is a fixed point

        ; ; Similarly:
        ;(lfirst '(
        ;        (Lambda a (Lambda b (Lambda c ((a b) c))))
        ;        (Lambda d (Lambda e (d e))) ))
        ;(lnext '(Apply (EvalResult ((Lambda a (Lambda b (Lambda c ((a b) c)))) (Lambda d (Lambda e (d e)))) () Done)))
        ;(lnext '(Apply (CloseFunc (Lambda a (Lambda b (Lambda c ((a b) c)))) (Lambda d (Lambda e (d e))) () Done)))
        ;(lnext '(Apply (EvalArg ((Lambda a (Lambda b (Lambda c ((a b) c))))) (Lambda d (Lambda e (d e))) () Done)))
        ;(lnext '(Apply (CloAtArg ((Lambda a (Lambda b (Lambda c ((a b) c))))) ((Lambda d (Lambda e (d e)))) () Done)))
        ;(lnext '(Apply (EvalResult (Lambda b (Lambda c ((a b) c))) ((a ((Lambda d (Lambda e (d e)))))) Done)))
        ;(lnext '(Result ((Lambda b (Lambda c ((a b) c))) (a ((Lambda d (Lambda e (d e))))))))
        ;
        ; ; Simplification of this closure is ours to do by hand or to automate further:
        ; ; = Lambda b (Lambda c ((Lambda e (b e)) c))
        ; ; = Lambda b (Lambda c (b c))
        
        ;(leval '(a b) '((a ((Lambda d (Lambda e (d e)))))))
        ;(lnext '(Apply (CloseFunc a b ((a ((Lambda d (Lambda e (d e)))))) Done)))
        (lnext '(Apply (EvalArg ((Lambda d (Lambda e (d e)))) b ((a ((Lambda d (Lambda e (d e)))))) Done)))
    )

'car
'(lambda (x) (car (cdr x)))
'(lambda (x) (car (cdr (cdr x))))
'(lambda (x) (car (cdr (cdr (cdr x)))))
'(lambda (x) (car (cdr (cdr (cdr (cdr x))))))
'cdr
'(lambda (x) (eq x '()))
'(lambda (x y) (cond (x y) ('t '())))



; Pattern matching, quasiquoting or at least "list", and "let" would make this simple.

; lapply
'(lambda (f x) (cons 'Apply (cons
    (cons 'EvalResult (cons (third (first f)) (cons
        (cons
            (cons (second (first f)) (cons x '()))
            (rest f) )
        (cons 'Done '()) )))
    '() )))

'(label lassoc (lambda (x e) (cond
    ((null e) '(Error one))
    ((atom e) '(Error three))
    ((atom (first e)) '(Error three))
    ((eq x (first (first e))) (cond
        ((atom (rest (first e))) '(Error two))
        ('t (cons 'Result (cons (second (first e)) '()))) ))
    ('t (lassoc x (rest e))) )))

; leval
'(lambda (x e) (cond
    ((null x) '(Error four))
    ((atom x) (lassoc x e))
    ((eq (first x) 'Lambda) (cons 'Result (cons (cons x e) '())))
    ((null (first x)) '(Error four))
    ((atom (rest x)) '(Error four))
    ('t (cons 'Apply (cons
        (cons 'CloseFunc (cons (first x) (cons (second x) (cons e (cons 'Done '())))))
        '() ))) ))

; 1
; lfirst
'(lambda (x) (cons 'Apply (cons (cons 'EvalResult (cons x (cons '() (cons 'Done '())))) '())))

; lnext
'(lambda (state) (cond
    ((eq (first state) 'Apply) (cond
        ((atom (second state)) '(Error zero))
        ((eq (first (second state)) 'EvalResult) (
            (lambda (state1) (cond
            
                ; 2
                ((and (eq (first state1) 'Apply) (eq (first (second state1)) 'CloseFunc)) (cons
                    'Apply
                    (cons
                        (cons 'CloseFunc (cons (second (second state1)) (cons
                            (third (second state1))
                            (cons (fourth (second state1)) (cons
                                (fourth (second state))
                                '() )) )))
                        '() ) ))
                ((eq (first state1) 'Result) (
                    (lambda (k) (cond
                        ((eq k 'Done) state1)
                        ((atom k) '(Error zero))
                    
                        ; 3
                        ((eq (first k) 'EvalArg) (cons 'Apply (cons
                            (cons 'EvalArg (cons (second state1) (rest (rest state1))))
                            '() )))
                        
                        ; 4
                        ((eq (first k) 'CloAtArg) (cons 'Apply (cons
                            (cons 'CloAtArg (cons (second k) (cons
                                (second state1)
                                (rest (rest (rest state1))) )))
                            '() )))
                        ('t '(Error zero)) ))
                    (fourth (second state)) ))
                ((eq (first state1) 'Error) state1)
                ('t '(Error zero)) ))
            (leval (second (second state)) (third (second state))) ))
        ((eq (first (second state)) 'CloseFunc) (
            (lambda (state1) (cond

                ; 5
                ((and (eq (first state1) 'Apply) (eq (first (second state1)) 'CloseFunc)) (cons
                    'Apply
                    (cons
                         (cons 'CloseFunc (cons (second (second state1)) (cons
                             (third (second state1))
                             (cons (fourth (second state1)) (cons
                                 (cons 'EvalArg (cons '() (rest (rest (second state)))))
                                 '() )) )))
                        '() ) ))
                
                ; 6
                ((eq (first state1) 'Result) (cons 'Apply (cons
                    (cons 'EvalArg (cons (second state1) (rest (rest (second state)))))
                    '() )))
                ((eq (first state1) 'Error) state1)
                ('t '(Error zero)) ))
            (leval (second (second state)) (fourth (second state))) ))
        ((eq (first (second state)) 'EvalArg) (
            (lambda (state1) (cond
                
                ; 7
                ((and (eq (first state1) 'Apply) (eq (first (second state1)) 'CloseFunc)) (cons
                    'Apply
                    (cons
                        (cons 'CloseFunc (cons (second (second state1)) (cons
                            (third (second state1))
                            (cons (fourth (second state1)) (cons
                                (cons 'CloAtArg (cons
                                    (second (second state))
                                    (cons '() (rest (rest (rest (second (state)))))) ))
                                '() )) )))
                        '() ) ))
                
                ; 8
                ((eq (first state1) 'Result) (cons 'Apply (cons
                    (cons 'CloAtArg (cons (second (second state)) (cons
                        (second state1)
                        (rest (rest (rest (second state)))) )))
                    '() )))
                ((eq (first state1) 'Error) state1)
                ('t '(Error zero)) ))
            (leval (third (second state)) (fourth (second state))) ))
        ; ((eq (first (second state)) 'CloAtArg) (   ; :-) :-)
        ('t (
            (lambda (state1) (cond
                
                ; 9
                ((and (eq (first state1) 'Apply) (eq (first (second state1)) 'EvalResult)) (cons
                    'Apply
                    (cons
                        (cons 'EvalResult (cons (second (second state1)) (cons
                            (third (second state1))
                            (cons (fifth (second state)) '()) )))
                        '() ) ))
                ((eq (first state1) 'Error) state1)
                ('t '(Error zero)) ))
            (lapply (second (second state)) (third (second state))) )) ))
    ('t state) )) ; an absorbing state: (Result v) or (Error n)

)
