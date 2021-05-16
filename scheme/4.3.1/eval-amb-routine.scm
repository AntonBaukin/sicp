
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

; As «amb-eval-define» for definition with the same name.
(define (amb-eval-inject var-name)
 (eval-env-define amb-evaluator-env var-name (eval var-name))
)

; This Gambit Scheme macros takes a script to evaluate
; being expressions then quoted. Using it allows you
; to write code as-is. See «eval-test.scm».
;
; Callback takes single argument: a value, or «void».
; It combines a pair of top-level success and fail
; callbacks. Value «void» means absence of the
; following values.
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
;
; The results are in FILO order (reversed)!
;
; Note: that the first item of the list is «void»
; in the case when all the variants were checked.
;
(define-macro (eval-amb-list . script)
 `(eval-amb-list-impl 100 '(,@script))
)

; List version of amb with explicit limit.
(define-macro (eval-amb-lim lim . script)
 `(eval-amb-lim-impl ,lim '(,@script))
)

; Same macros as «eval-amb-list», but it checks that the leading
; item of the resulting list is void (all variants are produced),
; and returns reversed tail-list — in the expected order.
;
(define-macro (eval-amb-results . script)
 `(eval-amb-results-impl (eval-amb-list-impl 100 '(,@script)))
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
 (define LIMIT limit)
 (define result '())
 (define acc '())

 (define (callback value)
  (if (eq? void value)
   (begin
    (set! limit LIMIT)
    (set! result (cons void acc))
    (set! acc '())
   )
   (begin
    (set! limit (- limit 1))
    (set! result '())
    (set! acc (cons value acc))
   )
  )

  (> limit 0) ;<— break if the limit is reached
 )

 (amb-evaluator callback script)
 (if (= limit 0) acc result)
)

(define (eval-amb-lim-impl limit script)
 (define results (eval-amb-list-impl limit script))

 (cond
  ((null? results)
   (error "Amb-evaluation produced nothing!")
  )

  ((eq? void (car results))
   (reverse (cdr results))
  )

  (else (reverse results))
 )
)

(define (eval-amb-results-impl results)
 (cond
  ((null? results)
   (error "Amb-evaluation produced nothing!")
  )

  ((not (eq? void (car results)))
   (error "Amb-evaluation gained the limit!")
  )

  (else (reverse (cdr results)))
 )
)
