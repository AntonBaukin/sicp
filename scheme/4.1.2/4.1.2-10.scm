(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/eval-impl-forms.scm"
  ; Here we overwrite definitions from «eval-impl-defs.scm»
  ; leaving dispatching evaluator implementation the same.
  "../4.1.2/4.1.2-10-defs.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

(assert-eq? #t
 (eval-basic (10 < 20))
)

(assert-eq? #f
 (eval-basic (20 < 10))
)

(assert-eq? 'B
 (eval-basic
  (define (smaller? a b) (< a b))
  (if (20 smaller? 10) 'A 'B)
 )
)

(assert-eq? 5
 (eval-basic
  (define (len lst n)
   (if (null? lst) n
    (len (cdr lst) (n + 1))
   )
  )

  (len '(1 2 3 4 5) 0)
 )
)

(assert-eq? 5
 (eval-basic
  (define (len seq)
   (define l 0)

   (for-each
    (lambda (item)
     (l := (l + 1))
    )
    seq
   )

   l
  )

  (len '(1 2 3 4 5))
 )
)

(assert-equal? 55
 (eval-basic
  (define (fib n)
   (cond
    ((n = 0) 0)
    ((n = 1) 1)
    (else ((fib (n - 2)) + (fib (n - 1))))
   )
  )

  (fib 10)
 )
)
