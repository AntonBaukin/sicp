(include "2.5.3-polynomial-rat-cut.0.scm")

; Creates cut() function for sparse polynomials that
; reduce the terms as it's in 2.96 task.
(define (make-sparse-polynomial-cut-1)
 (define make-poly (list-ref polynomial-package 0))
 (define cut-0 (make-sparse-polynomial-cut-0))
 (define cut-int rational-cut-int)

 (define remainder-terms (list-ref cut-0 2))
 (define make-cut-terms (list-ref cut-0 4))
 (define terms-poly? (list-ref cut-0 5))


 (define (simple-pow res base n)
  (if (= n 0) res
   (simple-pow (mul res base) base (- n 1))
  )
 )

 (define (drop-safe n)
  (if (apply-generic-tagged? n)
   (num-call-result n)
   (if (integer? n) (exact n) n)
  )
 )

 ; For the given sparse terms passed to div
 ; and remainder operation, returns scale
 ; factor for pseudo-remainder, see 2.96 task.
 (define (calc-pseudo-scale p q)
  (let ((c (cdar q)) (o1 (caar p)) (o2 (caar q)))
   (simple-pow 1 c (+ 1 o1 (- o2)))
  )
 )

 (define (op-terms-by-iter op terms s res)
  (if (null? terms) res
   (op-terms-by-iter
    op (cdr terms) s
    (cons
     (cons (caar terms) (drop-safe (op (cdar terms) s)))
     res
    )
   )
  )
 )

 (define (mul-terms-by terms s)
  (reverse (op-terms-by-iter mul terms s '()))
 )

 (define (div-terms-by terms s)
  (reverse (op-terms-by-iter div terms s '()))
 )

 (define (cut-terms-coeff terms)
  (if (< (length terms) 2) terms
   (let* (
     (a (car terms))
     (b (cadr terms))
     (c (cut-int (cdr a) (cdr b)))
     (s (abs (/ (cdr a) (car c))))
    )
    (div-terms-by terms s)
   )
  )
 )

 (define (pseudoremainder-terms a b)
  (cut-terms-coeff
   (remainder-terms
    (mul-terms-by a (calc-pseudo-scale a b))
    b
   )
  )
 )

 (define (trace-gcd a b)
  (if (not trace-sparse-polynomial-cut) void
   (log "GCD " a " ÷ " b " Scale: "
    (if (null? b) "–" (calc-pseudo-scale a b))
   )
  )
 )

 (define (gcd-terms a b)
  (trace-gcd a b)
  (if (null? b) a
   (gcd-terms b (pseudoremainder-terms a b))
  )
 )

 (list
  (make-cut-terms gcd-terms)
  cut-0 ;<— we also return full 0-collection
 )
)
