
; Better version of generic zero than in task 2.5.1-2.80.
; Parameter «mine» approximates zero equality for reals.
(define (install-zero-package scope mine)
 ; Approximates zero for real numbers.
 (define (zero? n) (< (abs n) mine))

 ; Recursive call for this package.
 (define apply-general (apply-generic-scope-function scope))
 (define (general-zero? n)
  (if (number? n) (zero? n)
   (apply-general 'zero? n)
  )
 )

 ((apply-generic-scope-register scope)
  'zero? '(integer) (lambda (i) (= 0 i))

  ; Float point numbers may be roughly equal.
  'zero? '(number) zero?

  ; Rational number is strictly equal to zero
  ; only when the numerator is zero.
  'zero? '(rational) (lambda (r) (= 0 (cdr r)))

  ; Here we compare with complex with zero using
  ; recursive invocation of generic zero?.
  'zero? '(complex) (lambda (c)
   (and
    (general-zero? (car c))
    (general-zero? (cdr c))
   )
  )
 )

 general-zero? ;<— returns general zero? predicate
)
