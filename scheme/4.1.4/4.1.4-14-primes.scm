(include "../4.1.1/eval-impl-primes.scm")

(define eval-primes-hugo
 (
  (lambda () ;<— immediately invoked function
   (set! eval-primes
    (append
     eval-primes
     ; So. Hugo wants to define map as a primitive function.
     ; Let's run task file «4.1.4-14.scm» and see...
     (list 'map map)
    )
   )
  )
 )
)
