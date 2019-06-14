
(define (defined? var-symbol)
 (with-exception-handler
  (lambda (e) void)
  (lambda () (not (eq? void (eval var-symbol))))
 )
)

(define define-if-not-private)

(define (define-if-not var-symbol init)
 (if (defined? var-symbol) void
  (let ((value (init)))
   (set! define-if-not-private (lambda () value))
   (eval (list 'define var-symbol '(define-if-not-private)))
   value
  )
 )
)
