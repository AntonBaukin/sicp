(include "2.5.1-arithmetics.scm")
(include "2.5.1-2.79-equ.scm")

(define (log . args) (for-each display args) (newline))

; Install equ? implementation package.
(install-arithmetic-package 'arithmetics-equ install-arithmetics-equ-package)

(define =zero? (curry num-call 'zero))

((apply-generic-scope-register numbers-scope)
 ; Float point numbers may be roughly equal.
 'zero '(number) (lambda (n) (<= n 0.00001))

 ; Rational number is strictly equal to zero only when
 ; the numerator is zero. (We back-tag the number.)
 'zero '(rational) (lambda (r)
  (= 0 (num-call 'numerator (num-tag-set 'rational r)))
 )

 ; Here we directly compare with complex zero.
 'zero '(complex) (lambda (c)
  (equ? (make-complex-xy 0 0) (num-tag-set 'complex c))
 )
)


(define (test-zero n)
 (log (=zero? n) " ?: 0 === " (num->str n))
)

(test-zero (make-number 0))
(test-zero (make-number 0.0001))
(test-zero (make-number 0.00001))

(test-zero (make-rat 1 100000))
(test-zero (make-rat 0 1))

(test-zero (make-complex-xy 0 0))
(test-zero (make-complex-ra 0 (* 0.25 3.14159265359)))
(test-zero (make-complex-xy 1 0))
