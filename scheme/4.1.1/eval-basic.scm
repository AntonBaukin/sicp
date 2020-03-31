(include "eval.scm")

; Basic evaluator core files:
(define-value-if-not 'eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.1/eval-impl-basic.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.1/eval-impl-set.scm"
 )
)


; Global scope of the basic evaluator.
(define basic-evaluator-env (eval-make-env))

; Function instance of basic evaluator.
(define basic-evaluator
 (make-eval
  basic-evaluator-env
  eval-basic-includes
  (eval-in-nested-env eval-impl exp env)
 )
)

; This Gambit Scheme macros takes a script to evaluate
; being expressions then quoted. Using it allows you
; to write code as-is. See «eval-test.scm».
(define-macro (eval-basic . script)
 `(basic-evaluator '(,@script))
)

; Now define standard global routines, such as map...
(include "eval-basic-std.scm")
