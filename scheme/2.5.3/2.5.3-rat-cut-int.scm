
; Implements GCD and rational cut() function for
; integers and plain numbers (also, integers).
(define (install-rational-cut-int-package scope)

 (define (gcd-iter x y); <— x >= y
  (if (= 0 y) x
   (gcd-iter y (remainder x y))
  )
 )

 (define (gcd a b)
  (if (> a b) (gcd-iter a b) (gcd-iter b a))
 )

 (define (sign a) (if (< a 0) -1 +1))

 (define (cut-num a b)
  (let ((g (gcd (abs a) (abs b))))
   (cons
    (* (sign a) (sign b) (/ (abs a) g))
    (/ (abs b) g)
   )
  )
 )

 ((apply-generic-scope-register scope)
  'cut '(integer integer) cut-num
  'cut '(number  number ) cut-num
 )
)
