
(define eval-disp-form-quote
 (
  (lambda () ;<— immediately invoked function
   (define (quote-form exp)
    (define q (text-of-quotation exp))
    (lambda (env) q)
   )

   (eval-disp-register-form 'quote quote-form)
   quote-form ;<— resulting form
  )
 )
)

(define eval-disp-form-define
 (
  (lambda () ;<— immediately invoked function
   (define (define-form exp)
    (define n (define-get-variable exp))
    (define vp (eval-analyze (define-get-value exp)))

    (lambda (env)
     (define value (vp env))
     (define-variable n value env)
     value ;<— and return this value
    )
   )

   (eval-disp-register-form 'define define-form)
   define-form ;<— resulting form
  )
 )
)

(define eval-disp-form-if
 (
  (lambda () ;<— immediately invoked function
   (define (if-form exp)
    (define pp (eval-analyze (if-predicate exp)))
    (define cp (eval-analyze (if-consequent exp)))
    (define ap (eval-analyze (if-alternative exp)))
    
    (lambda (env)
     (if (pp env) (cp env) (ap env))
    )
   )

   (eval-disp-register-form 'if if-form)
   if-form ;<— resulting form
  )
 )
)

(define eval-disp-form-cond
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

(define eval-disp-form-set
 (
  (lambda () ;<— immediately invoked function
   (define (set-form exp)
    (define n (assignment-variable exp))
    (define vp (eval-analyze (assignment-value exp)))
    
    (lambda (env)
     (assign-variable env n (vp env))
    )
   )

   (eval-disp-register-form 'set! set-form)
   set-form ;<— resulting form
  )
 )
)

(define analyze-sequence
 (
  (lambda () ;<— immediately invoked function
   (define (seq a b)
    (lambda (env) (a env) (b env))
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

(define eval-disp-form-begin
 (
  (lambda () ;<— immediately invoked function
   (define (begin-form exp)
    (analyze-sequence (begin-actions exp))
   )

   (eval-disp-register-form 'begin begin-form)
   begin-form ;<— resulting form
  )
 )
)

(define eval-disp-form-global
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

    (lambda (env)
     (define value (vp env))
     (define-variable n value (get-global-env env))
     value ;<— and return this value
    )
   )

   (eval-disp-register-form 'global global-form)
   global-form ;<— resulting form
  )
 )
)

(define eval-disp-form-lambda
 (
  (lambda () ;<— immediately invoked function
   (define (lambda-form exp)
    (define ps (lambda-parameters exp))
    (define bp (analyze-sequence (lambda-body exp)))

    (lambda (env)
     (make-procedure ps bp env)
    )
   )

   (eval-disp-register-form 'lambda lambda-form)
   lambda-form ;<— resulting form
  )
 )
)

(define eval-disp-form-eval
 (
  (lambda () ;<— immediately invoked function
   (define (eval-form exp)
    (define ep (eval-analyze (cadr exp)))

    (lambda (env)
     (eval-impl (ep env) env)
    )
   )

   (eval-disp-register-form 'eval eval-form)
   eval-form ;<— resulting form
  )
 )
)

(define eval-disp-form-apply
 (
  (lambda () ;<— immediately invoked function
   (define (apply-form exp)
    (define fp (eval-analyze (cadr exp)))
    (define ap (eval-analyze (caddr exp)))

    (lambda (env)
     (apply-impl (fp env) (ap env) env exp)
    )
   )

   (eval-disp-register-form 'apply apply-form)
   apply-form ;<— resulting form
  )
 )
)

(define eval-disp-form-debug
 (
  (lambda () ;<— immediately invoked function
   (define (debug-form exp)
    (lambda (env) (debug-call exp env))
   )

   (eval-disp-register-form 'debug debug-form)
   debug-form ;<— resulting form
  )
 )
)

(define eval-disp-form-and
 (
  (lambda () ;<— immediately invoked function
   (define (next ps env)
    (if (null? ps) #t
     (if ((car ps) env)
      (next (cdr ps) env)
      #f
     )
    )
   )
   
   (define (and-form exp)
    (define ps (map eval-analyze (cdr exp)))
    (lambda (env) (next ps env))
   )

   (eval-disp-register-form 'and and-form)
   and-form ;<— resulting form
  )
 )
)

(define eval-disp-form-or
 (
  (lambda () ;<— immediately invoked function
   (define (next ps env)
    (if (null? ps) #f
     (if ((car ps) env)
      #t
      (next (cdr ps) env)
     )
    )
   )
   
   (define (or-form exp)
    (define ps (map eval-analyze (cdr exp)))
    (lambda (env) (next ps env))
   )

   (eval-disp-register-form 'or or-form)
   or-form ;<— resulting form
  )
 )
)

(define eval-disp-form-let
 (
  (lambda () ;<— immediately invoked function
   (define (let-form exp)
    ; The list of variable names created for «let»:
    (define vars (map car (cadr exp)))

    (define valps ;<— analyze the values
     (map
      (lambda (exp) (eval-analyze (cadr exp)))
      (cadr exp)
     )
    )

    ; Analyzed wrapped body:
    (define bodyp (eval-analyze (eval-wrap-begin (cddr exp))))

    (lambda (env) ;<— resulting runner
     ; Evaluate analyzed values:
     (define vals (map (lambda (vp) (vp env)) valps))

     ; Then we extend environment:
     (define env-ext (extend-environment vars vals env #f))

     ; Provide some info on the new environment:
     (if-debug (env-info-add env-ext 'let vars))

     (bodyp env-ext) ;<— run the analyzed body
    )
   )

   (eval-disp-register-form 'let let-form)
   let-form ;<— resulting form
  )
 )
)

; Special form «try» allows to call an expression with
; optional fallback one: (try expression [fallback]).
(define eval-disp-form-try
 (
  (lambda () ;<— immediately invoked function
   (define (try-form exp)
    (define ep (eval-analyze (cadr exp)))

    (define fp
     (if (null? (cddr exp)) '()
      (eval-analyze (caddr exp))
     )
    )

    (lambda (env)
     (with-exception-catcher
      ; Handle fallback expression:
      (lambda (e)
       ; Has no fallback? Or invoke it:
       (if (null? fp) void (fp e))
      )

      ; Try evaluate main expression:
      (lambda () (ep env))
     )
    )
   )

   (eval-disp-register-form 'try try-form)
   try-form ;<— resulting form
  )
 )
)
