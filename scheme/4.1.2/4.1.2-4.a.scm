(define (log . args) (for-each display args) (newline))

; Enable debug mode:
(define basic-evaluator-debug? #t)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; On how this works, see «eval-disp-register-gateway»
; in «eval-impl-disp.scm».
;
; Note: that each lambda is evaluated inside the evaluator,
; thus it has access to each special form, but it has no
; access to the evaluator internals.
;
(eval-basic (register
 and ;<— we do not to quote it

 (lambda (exp env)
  (define (next exp)
   (if (null? exp) #t
    (if (eval (car exp))
     (next (cdr exp))
     #f
    )
   )
  )

  (next (cdr exp))
 )

 or ;<— we do not to quote it

 (lambda (exp env)
  (define (next exp)
   (if (null? exp) #f
    (if (eval (car exp))
     #t
     (next (cdr exp))
    )
   )
  )

  (next (cdr exp))
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
