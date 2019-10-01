
(define (install-arithmetics-equ-package scope)
 (define NTAG '(number number))
 (define RTAG '(rational rational))
 (define CTAG '(complex complex))

 ; Float point numbers may be roughly equal.
 (define (eq a b)
  (< (abs (- a b)) 0.00001)
 )

 (define rat-num (curry num-call 'numerator))
 (define rat-den (curry num-call 'denominator))

 (define (eq-rat a b)
  ; We have to wrap the numbers back to apply generic.
  (let (
    (ta (num-tag-set (car RTAG) a))
    (tb (num-tag-set (car RTAG) b))
   )

   ; As rational numbers are strict, no threshold may be.
   (=
    (* (rat-num ta) (rat-den tb))
    (* (rat-num tb) (rat-den ta))
   )
  )
 )

 (define (eq-complex a b)
  ; Here we take special exports from the package.
  (define real-part (list-ref complex-package 1))
  (define imag-part (list-ref complex-package 2))

  (and
   (eq (real-part a) (real-part b))
   (eq (imag-part a) (imag-part b))
  )
 )

 ((apply-generic-scope-register scope)
  'equ NTAG eq
  'equ RTAG eq-rat
  'equ CTAG eq-complex
 )
)

(define equ? (curry num-call 'equ))
