
; Unline SICP, we do not treat specially primitive
; procedures. Instead, we install them in the global
; environment — thus we allow to redefine them as it's
; possible in Gambit Scheme.
(define (apply-basic procedure arguments)
 (if (compound-procedure? procedure)
  (eval-sequence
   (procedure-body procedure)
   (extend-environment
    (procedure-parameters procedure)
    arguments
    (procedure-environment procedure)
   )
  )
  (underlying-apply procedure arguments)
 )
)

(define (eval-sequence exps env)
 (if (null? (cdr exps))
  (eval-impl (car exps) env)
  (begin
   (eval-impl (car exps) env)
   (eval-sequence (cdr exps) env)
  )
 )
)
