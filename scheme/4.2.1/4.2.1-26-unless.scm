
(define eval-disp-form-unless
 (
  (lambda () ;<— immediately invoked function
   (define (unless-form exp)
    (define pp (eval-analyze (if-predicate exp)))
    (define cp (eval-analyze (if-consequent exp)))
    (define ap (eval-analyze (if-alternative exp)))

    ; «Unless» form differs from «if» one only
    ; in the order of the branches:
    (lambda (env)
     (if (pp env) (ap env) (cp env))
    )
   )

   (eval-disp-register-form 'unless unless-form)
   unless-form ;<— resulting form
  )
 )
)
