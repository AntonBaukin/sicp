
; Special form «resolve» forces thunks resolve.
(define eval-resolve-form
 (
  (lambda () ;<— immediately invoked function
   (define (resolve-form exp)
    (define p (eval-analyze (cadr exp)))
    (lambda (env) (resolve-value (p env)))
   )

   (eval-disp-register-form 'resolve resolve-form)
   resolve-form ;<— resulting form
  )
 )
)

; Special form «memoff» wraps expression into
; a thunk that is not memoized.
(define eval-memoff-form
 (
  (lambda () ;<— immediately invoked function
   (define (memoff-form exp)
    (define p (eval-analyze (cadr exp)))
    (lambda (env) (make-thunk p env #f))
   )

   (eval-disp-register-form 'memoff memoff-form)
   memoff-form ;<— resulting form
  )
 )
)
