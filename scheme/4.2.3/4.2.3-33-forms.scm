
(define eval-disp-form-quote-lists
 (
  (lambda () ;<— immediately invoked function
   (define (quote-form exp)
    (define q (text-of-quotation exp))
    (define exq (list 'native->pairs q))

    (lambda (env)
     (apply-basic-compound
      (lookup-variable 'native->pairs env)
      (list q)
      env
      exq
     )
    )
   )

   (eval-disp-register-form 'quote quote-form)
   quote-form ;<— resulting form
  )
 )
)
