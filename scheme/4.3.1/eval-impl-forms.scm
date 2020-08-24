
(define eval-amb-form-quote
 (
  (lambda () ;<— immediately invoked function
   (define (quote-form exp)
    (define q (text-of-quotation exp))
    (lambda (success fail env) (success fail q))
   )

   (eval-disp-register-form 'quote quote-form)
   quote-form ;<— resulting form
  )
 )
)

(define eval-amb-form-if
 (
  (lambda () ;<— immediately invoked function
   (define (if-form exp)
    (define pp (eval-analyze (if-predicate exp)))
    (define cp (eval-analyze (if-consequent exp)))
    (define ap (eval-analyze (if-alternative exp)))

    (lambda (success fail env)
     (pp
      (lambda (fail2 flag)
       (if flag
        (cp success fail2 env)
        (ap success fail2 env)
       )
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'if if-form)
   if-form ;<— resulting form
  )
 )
)

(define eval-amb-form-cond
 (
  (lambda () ;<— immediately invoked function
   (define (cond-form exp)
    (eval-analyze (cond->if exp))
   )

   (eval-disp-register-form 'cond cond-form)
   cond-form ;<— resulting form
  )
 )
)

(define eval-amb-form-define
 (
  (lambda () ;<— immediately invoked function
   (define (define-form exp)
    (define n (define-get-variable exp))
    (define vp (eval-analyze (define-get-value exp)))

    (lambda (success fail env)
     (vp
      (lambda (fail2 value)
       (define-variable n value env)
       (success fail2 value) ;<— and return this value
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'define define-form)
   define-form ;<— resulting form
  )
 )
)

; Definition in global environment.
(define eval-amb-form-global
 (
  (lambda () ;<— immediately invoked function
   (define (global-form exp)
    (define n (define-get-variable exp))

    ; By default name is the same name symbol:
    (define vp
     (eval-analyze
      (if (= 2 (length exp))
       n
       (define-get-value exp)
      )
     )
    )

    (lambda (success fail env)
     (vp
      (lambda (fail2 value)
       (define-variable n value (get-global-env env))
       (success fail2 value) ;<— and return this value
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'global global-form)
   global-form ;<— resulting form
  )
 )
)

(define eval-amb-form-lambda
 (
  (lambda () ;<— immediately invoked function
   (define (lambda-form exp)
    (define ps (lambda-parameters exp))
    (define bp (analyze-amb-sequence (lambda-body exp)))

    (lambda (success fail env)
     (success fail (make-procedure ps bp env))
    )
   )

   (eval-disp-register-form 'lambda lambda-form)
   lambda-form ;<— resulting form
  )
 )
)

(define eval-amb-form-debug
 (
  (lambda () ;<— immediately invoked function
   (define (debug-form exp)
    (lambda (success fail env)
     (success fail (debug-call exp env))
    )
   )

   (eval-disp-register-form 'debug debug-form)
   debug-form ;<— resulting form
  )
 )
)


(define eval-amb-form-set
 (
  (lambda () ;<— immediately invoked function
   (define (set-form exp)
    (define n (assignment-variable exp))
    (define vp (eval-analyze (assignment-value exp)))

    (lambda (success fail env)
     (vp
      (lambda (fail2 value)
       (define old (lookup-variable n env))
       (assign-variable env n value)

       (success
        (lambda ()
         (assign-variable env n old)
         (fail2)
        )
        value
       )
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'set! set-form)
   set-form ;<— resulting form
  )
 )
)

(define eval-amb-form-begin
 (
  (lambda () ;<— immediately invoked function
   (define (begin-form exp)
    (analyze-amb-sequence (begin-actions exp))
   )

   (eval-disp-register-form 'begin begin-form)
   begin-form ;<— resulting form
  )
 )
)

(define eval-amb-form-apply
 (
  (lambda () ;<— immediately invoked function
   (define (apply-form exp)
    (define fp (eval-analyze (cadr exp)))
    (define ap (eval-analyze (caddr exp)))

    (lambda (success fail env)
     (fp
      (lambda (fail2 proc)
       (ap
        (lambda (fail3 args)
         (apply-impl success fail3 proc args env exp)
        )
        fail2
        env
       )
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'apply apply-form)
   apply-form ;<— resulting form
  )
 )
)

(define eval-amb-form-and
 (
  (lambda () ;<— immediately invoked function
   (define (next success fail ps env)
    (if (null? ps)
     (success fail #t)
     ((car ps)
      (lambda (fail2 flag)
       (if flag
        (next success fail2 (cdr ps) env)
        (success fail2 #f)
       )
      )
      fail
      env
     )
    )
   )

   (define (and-form exp)
    (define ps (map eval-analyze (cdr exp)))
    (lambda (success fail env) (next success fail ps env))
   )

   (eval-disp-register-form 'and and-form)
   and-form ;<— resulting form
  )
 )
)

(define eval-amb-form-or
 (
  (lambda () ;<— immediately invoked function
   (define (next success fail ps env)
    (if (null? ps)
     (success fail #f)
     ((car ps)
      (lambda (fail2 flag)
       (if flag
        (success fail2 #t)
        (next success fail2 (cdr ps) env)
       )
      )
      fail
      env
     )
    )
   )

   (define (or-form exp)
    (define ps (map eval-analyze (cdr exp)))
    (lambda (success fail env) (next success fail ps env))
   )

   (eval-disp-register-form 'or or-form)
   or-form ;<— resulting form
  )
 )
)

(define eval-amb-form-let
 (
  (lambda () ;<— immediately invoked function
   (define (let-form exp)
    ; The list of variable names created for «let»:
    (define vars (map car (cadr exp)))

    ; Analyzed wrapped body:
    (define bp (analyze-amb-sequence (cddr exp)))

    (define aps ;<— analyze the values
     (map
      (lambda (exp) (eval-analyze (cadr exp)))
      (cadr exp)
     )
    )

    (lambda (success fail env) ;<— execution procedure
     (amb-apply-args
      (lambda (fail2 vals)
       (define env-ext (extend-environment vars vals env #f))

       ; Provide some info on the new environment:
       (if-debug (env-info-add env-ext 'let vars))

       (bp success fail2 env-ext) ;<— run the analyzed body
      )
      fail
      aps
      env
     )
    )
   )

   (eval-disp-register-form 'let let-form)
   let-form ;<— resulting form
  )
 )
)

(define eval-amb-form-amb
 (
  (lambda () ;<— immediately invoked function
   (define (amb-form exp)
    (define ps (map eval-analyze (cdr exp)))

    (lambda (success fail env) ;<— execution procedure
     (define (try-next choices)
      (if (null? choices)
       (fail)
       ((car choices) ;<— call p-ith executor
        success
        (lambda () (try-next (cdr choices)))
        env
       )
      )
     )

     (try-next ps)
    )
   )

   (eval-disp-register-form 'amb amb-form)
   amb-form ;<— resulting form
  )
 )
)

;(define eval-amb-form-results
; (
;  (lambda () ;<— immediately invoked function
;   (define (results-form exp)
;    (define sp (analyze-amb-sequence (cdr exp)))
;
;    (lambda (success fail env) ;<— execution procedure
;     (define (success-result fail2 val)
;      (success fail2 (cons 'result val))
;     )
;
;     (sp success-result fail env)
;    )
;   )
;
;   (eval-disp-register-form 'results results-form)
;   results-form ;<— resulting form
;  )
; )
;)
