
; Binds first arguments of the function with the given.
(define (curry f . xargs)
 (lambda (arg . rest)
  (apply f (append xargs (cons arg rest)))
 )
)
