(include "try-finally.scm")

(define (make-serializer)
 (define mutex (make-mutex))

 (lambda (f)
  (define (wrapper . args)
   (mutex-lock! mutex)

   (try-finally
    (lambda () (apply f args))
    (lambda () (mutex-unlock! mutex))
   )
  )

  wrapper ;<— resulting critical section
 )
)
