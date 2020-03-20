(define (log . args) (for-each display args) (newline))

; Enable debug mode:
(define basic-evaluator-debug? #t)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Alternative variant of task 4 of §4.1.2.
; See direct evaluation in «4.1.2-4.a.scm».
;
; Here we transform «and» and «or» expressions
; into if-clauses.
;
(eval-basic (register
 and ;<— we do not to quote it

 (lambda (exp env)
  (define (next exp)
   (if (null? exp) #t
    (list
     'if
     (car exp)
     (next (cdr exp))
     #f
    )
   )
  )

  (eval (next (cdr exp)))
 )

 or ;<— we do not to quote it

 (lambda (exp env)
  (define (next exp)
   (if (null? exp) #f
    (list
     'if
     (car exp)
     #t
     (next (cdr exp))
    )
   )
  )

  (eval (next (cdr exp)))
 )
))

(assert-true?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 2))
 )
)

(assert-false?
 (eval-basic
  (and (= 1 2) (error "Wrong!"))
 )
)

(assert-true?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 2))
 )
)

(assert-true?
 (eval-basic
  (or (= 1 1) (error "Wrong!"))
 )
)
