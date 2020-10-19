
(define eval-amb-form-require
 (
  (lambda () ;<— immediately invoked function
   (define (require-form exp)
    (define predicatep (eval-analyze (cadr exp)))

    (lambda (success fail env)
     (predicatep ;<— evaluate the predicate
      (lambda (fail2 valid?)
       (if valid?
        (success fail2 valid?)
        (fail)
       )
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'REQUIRE require-form)
   require-form ;<— resulting form
  )
 )
)
