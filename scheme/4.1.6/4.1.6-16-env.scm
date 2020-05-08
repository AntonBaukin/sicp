
(define lookup-variable-unassigned
 (
  (lambda () ;<— immediately invoked function
   (define (lookup var env)
    (define value (lookup-variable-optional var env))

    (cond
     ((eq? void value)
      (lookup-special-global var env)
     )

     ((eq? '*unassigned* value)
      (error "Using unassigned variable" var)
     )

     (else value)
    )
   )

   (set! lookup-variable lookup)
  )
 )
)
