
; Higher-order function that wrap the given one
; and always returns void. Useful in iteration
; callbacks that do require void-returning
; visitors not to break the iteration.
(define (return-void f)
 (define (wrap . args)
  (apply f args)
  void
 )

 wrap ;<— resulting function
)
