
; Global scope of the amb evaluator:
(define amb-evaluator-env (eval-make-env))

; Function instance of basic evaluator.
(define amb-evaluator
 (make-eval
  amb-evaluator-env
  eval-amb-includes
  ; Instead of normal evaluation, we call amb-one:
  (eval-amb-in-nested-env callback eval-impl exp env)
 )
)

; Injects variable into global environment.
(define (amb-eval-define var-name value)
 (eval-env-define amb-evaluator-env var-name value)
)

; This Gambit Scheme macros takes a script to evaluate
; being expressions then quoted. Using it allows you
; to write code as-is. See «eval-test.scm».
;
; Callback takes single argument: a value, or «void».
; It combines a pair of top-level success and fail
; callbacks. Value «void» means absence of the
; vallowing values.
;
; When callback returns «#t», it asks to continue
; the evaluation via failure continuation. In this
; it resembles try-again call from the driver loop
; in SICP. Instead of relying of user input, this
; variant provides pure programmatic driving.
;
(define-macro (eval-amb callback . script)
 `(amb-evaluator ,callback '(,@script))
)

; Runs amb evaluation and breaks it on the first result.
(define-macro (eval-basic . script)
 `(eval-amb-result-impl '(,@script))
)

; Runs amb evaluation and collects a list of results
; with the pre-defined limit of 1000 items.
(define-macro (eval-amb-list . script)
 `(eval-amb-list-impl 1000 '(,@script))
)

(define (eval-amb-result-impl script)
 (define result void)

 (define (callback value)
  (set! result value)
  #f ;<— explicitly ask to break
 )

 (amb-evaluator callback script)
 result
)

(define (eval-amb-list-impl limit script)
 (define result '())

 (define (callback value)
  (set! result (cons value result))
  (set! limit (- limit 1))
  (> limit 0) ;<— break if the limit is reached
 )

 (amb-evaluator callback script)
 (reverse result)
)
