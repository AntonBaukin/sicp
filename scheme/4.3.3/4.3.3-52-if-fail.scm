
(define eval-amb-form-if-fail
 (
  (lambda () ;<— immediately invoked function
   (define (if-fail-form exp)
    (define successp (eval-analyze (cadr exp)))
    (define failp (eval-analyze (caddr exp)))
    (define got #f) ;<— true if at least one succeed

    (lambda (success fail env)
     (successp
      (lambda (fail2 value)
       (set! got #t) ;<— got it!
       (success fail2 value)
      )
      (lambda () ; <— fallback branch
       (if got
        (fail)
        (failp success fail env)
       )
      )
      env
     )
    )
   )

   (eval-disp-register-form 'if-fail if-fail-form)
   if-fail-form ;<— resulting form
  )
 )
)
