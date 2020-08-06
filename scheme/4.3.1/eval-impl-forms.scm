
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
