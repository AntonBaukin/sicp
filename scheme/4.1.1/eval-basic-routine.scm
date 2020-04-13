
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
