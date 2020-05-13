
; To implement Eve's idea on variables resolve, we replace
; *unassigned* marker with promised expression.
(define lookup-variable-promised
 (
  (lambda () ;<— immediately invoked function
   (define (promised? value)
    (and
     (pair? value)
     (eq? 'promised (car value))
     (eval-promise? (cdr value))
    )
   )

   (define (resolve value)
    (eval-force (cdr value))
   )

   (define (lookup var env)
    (define value (lookup-variable-optional var env))

    (cond
     ((eq? void value)
      (lookup-special-global var env)
     )

     ((promised? value)
      (resolve value)
     )

     (else value)
    )
   )

   (set! lookup-variable lookup)
  )
 )
)
