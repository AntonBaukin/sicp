
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

(define (define-value-if-not var-symbol value)
 (define-if-not var-symbol (lambda () value))
)

; Invokes the global function defined by name fun-symbol,
; or else function (that may be void) with the given arguments.
(define (if-defined?-else fun-symbol else-fun . args)
 (if (not (defined? fun-symbol))
  (if (eq? void else-fun) void (apply else-fun args))
  (let ((f (eval fun-symbol)))
   (apply f args)
  )
 )
)
