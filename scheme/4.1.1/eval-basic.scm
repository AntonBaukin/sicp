(include "eval.scm")

; Basic evaluator core files:
(define-value-if-not 'eval-basic-includes
  '(
    "../4.1.1/eval-impl-env.scm"
    "../4.1.1/eval-impl-defs.scm"
    "../4.1.1/eval-impl-apply.scm"
    "../4.1.1/eval-impl-basic.scm"
    "../4.1.1/eval-impl-debug.scm"
    "../4.1.1/eval-impl-primes.scm"
   )
)


; Global scope of the basic evaluator.
(define basic-evaluator-env (eval-make-env))

; Function instance of basic evaluator.
(define basic-evaluator
 (make-eval
  basic-evaluator-env
  eval-basic-includes
  (eval-in-nested-env eval-basic exp env)
 )
)

; This Gambit Scheme macros takes a script to evaluate
; being expressions then quoted. Using it allows you
; to write code as-is. See «eval-test.scm».
(define-macro (eval-basic . script)
 `(eval-basic-impl '(,@script))
)

; Here we evaluate the script line-by-line that allows
; us to mix top-level expressions in any way, just any
; script is executed.
(define (eval-basic-impl script)
 (cond
  ((null? script) '())
  ((null? (cdr script))
   (basic-evaluator (car script))
  )
  (else
   (basic-evaluator (car script))
   (eval-basic-impl (cdr script))
  )
 )
)
