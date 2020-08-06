;
; Implementation of nondeterministic evaluator.
;

; See «eval-in-nested-env()» in «4.1.1/eval-impl-env.scm».
;
; This version alters the behaviour of the evaluation to
; meets the peculiarity of amb-evauation.
;
; Instead of returning the result being the last expression
; evaluated, it creates success and fail pair that invoke
; the callback given — see «eval-amb-routine.scm».
;
(define (eval-amb-in-nested-env callback evaluator script env)
 (define nested (eval-extend-env env))

 ; Success callback drives the continuation when asked.
 ; (Note the order of arguments — reversed of SICP.)
 (define (success fail value)
  (if (eq? #t (callback value)) (fail))
 )

 (define (fail) (callback void))

 (if-debug
  (env-info-add nested 'eval-private-scope)
  (if (env-info-miss? env 'global)
   (env-info-add env 'global)
  )
 )

 (for-each
  (lambda (exp) (evaluator success fail exp nested))
  script
 )
)

; Note that, unlike SICP, we pass success and fail pair
; as the leading arguments, not the trailing.
(define (eval-amb success fail exp env)
 ((eval-analyze exp) success fail env)
)

(define (eval-analyze exp)
 (cond
  ((self-evaluating? exp)
   (lambda (success fail env) (success fail exp))
  )

  ((direct-value? exp)
   (eval-analyze-direct-value exp)
  )

  ((variable? exp)
   (lambda (success fail env) (success fail (lookup-variable exp env)))
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
 (lambda (success fail env) (success fail value))
)

(define (eval-dispatch exp)
 (define form (eval-lookup-form (car exp)))

 (if (not (eq? void form))
  (form exp)
  (eval-disp-else exp)
 )
)

(define (eval-disp-else exp)
 (eval-amb-apply exp)
)

; It's «get-args» of SICP.
(define (amb-apply-args success fail aps env)
 (if (null? aps)
  (success fail '())
  ((car aps) ;<— call current argument-proc
   (lambda (fail2 arg) ;<— success2
    (amb-apply-args
     (lambda (fail3 args)
      (success fail3 (cons arg args))
     )
     fail2
     (cdr aps)
     env
    )
   )
   fail
   env
  )
 )
)

(define (eval-amb-apply exp)
 (define fp (eval-analyze (operator exp)))
 (define aps (map eval-analyze (operands exp)))

 (lambda (success fail env)
  (fp
   (lambda (fail2 procedure) ;<— success2
    (amb-apply-args
     (lambda (fail3 args) ;<— success3
      (apply-impl success fail3 procedure args env exp)
     )
     fail2
     aps
     env
    )
   )
   fail
   env
  )
 )
)

(define (apply-amb success fail procedure arguments env exp)
 (if (compound-procedure? procedure)
  (apply-basic-compound success fail procedure arguments env exp)
  (success fail (underlying-apply procedure arguments))
 )
)

(define (apply-basic-compound success fail procedure arguments env exp)
 (define nest? (same-procedure-env? env procedure))

 (define call-env
  (extend-environment
   (procedure-parameters procedure)
   arguments
   (if nest? env (procedure-environment procedure))
   nest?
  )
 )

 (if (not nest?)
  (begin
   ; Tag the environment with the procedure name:
   (if (symbol? (operator exp))
    (env-info-add call-env (operator exp))
   )

   ; Allow track recursive calls:
   (env-info-add call-env procedure)
  )
 )

 ((procedure-body procedure) success fail call-env)
)

(define analyze-amb-sequence
 (
  (lambda () ;<— immediately invoked function
   (define (seq a b)
    (lambda (success fail env)
     (a
      (lambda (fail2 a-value) ;<— value is not used
       (b success fail2 env)
      )
      fail
      env
     )
    )
   )

   (define (join exps)
    (if (null? (cdr exps))
     (eval-analyze (car exps))
     (seq
      (eval-analyze (car exps))
      (join (cdr exps))
     )
    )
   )

   join ;<— resulting join
  )
 )
)

; Copied from «4.1.1/eval-impl-apply.scm».
(define (same-procedure-env? env procedure)
 (env-info-has? env procedure)
)

(define debug-log-eval-amb-msg-value
 (
  (lambda () ;<— immediately invoked function
   (define (fail) void)

   (define (msg-value exp env)
    (define result void)
    (define (success fail value) (set! result value))
    (eval-impl success fail exp env)
    result ;<— resulting value
   )

   (set! debug-log-eval-msg-value msg-value)
   msg-value ;<— resulting function
  )
 )
)
