
(define (eval-disp exp env)
 ((eval-analyze exp) env)
)

; This analyzer resembles the same as the ones
; from §4.1.2, and §4.1.6.
(define (eval-analyze exp)
 (cond
  ((self-evaluating? exp)
   (lambda (env) exp)
  )

  ((direct-value? exp)
   (eval-analyze-direct-value exp)
  )

  ((variable? exp)
   (lambda (env) (lookup-variable exp env))
  )

  ; Delegation to the dispatcher:
  ((eval-form? exp)
   (eval-dispatch exp)
  )

  (else (eval-disp-else exp))
 )
)

(define (eval-form? exp)
 (and (list? exp) (symbol? (car exp)))
)

(define (eval-analyze-direct-value exp)
 (define value (unwrap-direct-value exp))
 (lambda (env) value)
)

(define (eval-dispatch exp)
 (define form (eval-lookup-form (car exp)))

 (if (not (eq? void form))
  (form exp)
  (eval-disp-else exp)
 )
)

(define (eval-disp-else exp)
 (eval-disp-apply exp)
)

(define (eval-disp-apply exp)
 (define fp (eval-analyze (operator exp)))
 (define aps (map eval-analyze (operands exp)))

 (lambda (env)
  (apply-impl
   (fp env)
   (map (lambda (ap) (ap env)) aps)
   env
   exp
  )
 )
)

; Rewrite «apply-compound-body()» from «4.1.1/eval-impl-apply.scm».
(define apply-compound-body-analyzed
 (
  (lambda () ;<— immediately invoked function
   (define (apply-analyzed procedure env)
    ((procedure-body procedure) env)
   )

   (set! apply-compound-body apply-analyzed)
   apply-analyzed ;<— resulting function
  )
 )
)
