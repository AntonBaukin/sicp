
(define (make-semaphore n)
 (define mutex (make-mutex))
 (define await (make-condition-variable))

 (define (acquire)
  (mutex-lock! mutex)
  (if (> n 0)
   (begin
    (set! n (- n 1))
    (mutex-unlock! mutex)
   )

   ; We unlock mutex using conditional variable
   ; to make the thread sleep. Note, that in SICP
   ; there is no a word about how to make thread
   ; sleep, wait it, — and complex stuff related!
   (begin
    (mutex-unlock! mutex await)
    (acquire) ;<— try again!
   )
  )
 )

 (define (release)
  (mutex-lock! mutex)
  (set! n (+ n 1))
  (condition-variable-signal! await)
  (mutex-unlock! mutex)
 )

 (if (not (> n 0))
  (error "Illegal semaphore number!" n)
 )

 (list acquire release) ;<— resulting ops
)

(define (semaphore-acquire sem)
 ((car sem))
)

(define (semaphore-release sem)
 ((cadr sem))
)
