(define eval-amb-form-pset
 (
  (lambda () ;<— immediately invoked function
   (define (pset-form exp)
    (define n (assignment-variable exp))
    (define vp (eval-analyze (assignment-value exp)))

    (lambda (success fail env)
     (vp
      (lambda (fail2 value)
       (assign-variable env n value)
       ; Just not reset the value back in nested fail-branch:
       (success fail2 value)
      )
      fail
      env
     )
    )
   )

   (eval-disp-register-form 'permanent-set! pset-form)
   pset-form ;<— resulting form
  )
 )
)
