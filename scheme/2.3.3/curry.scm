
; Binds first arguments of the function with the given.
(define (curry f . xargs)
 (define (curried . args)
  (apply f (append xargs args))
 )

 curried ;<— resulting function
)
