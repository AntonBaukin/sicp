(include "2.5.1-arithmetics.scm")
(include "2.5.1-2.79-equ.scm")

(define (log . args) (for-each display args) (newline))

; Install equ? implementation package.
(install-arithmetic-package 'arithmetics-equ install-arithmetics-equ-package)

(define (test-equ a b)
 (log (equ? a b) " ?: " (num->str a) " === " (num->str b))
)

(test-equ (make-number 1) (make-number 1))
(test-equ (make-number 1) (make-number 2))
(test-equ (make-number 1) (make-number 0.999))
(test-equ (make-number 1) (make-number 0.99999))

(test-equ (make-rat 1 2) (make-rat 1 2))
(test-equ (make-rat 1 2) (make-rat 50000 99999))

(define pi 3.14159265359)

; Two same complex numbers defined in rect and polar systems.
(define xy1_1 (make-complex-xy 1 1))
(define ra_sq2_45 (make-complex-ra (sqrt 2) (* 0.25 pi)))

(test-equ xy1_1 ra_sq2_45)
