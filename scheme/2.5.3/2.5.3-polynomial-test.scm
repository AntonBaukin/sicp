(include "2.5.3-arithmetics.scm")
(include "2.5.3-polynomial.scm")

(define (log . args) (for-each display args) (newline))

(install-arithmetic-package
 'polynomial-package
 install-polynomial-package
)

(define N make-number)  ;<— shortcuts...
(define I make-integer)
(define P (cadr polynomial-package))

(define (log-poly p)
 (log p " = " (num->str p))
)

(log "2x + 3 = " (P 'x 1 2 0 3))

; Prints 4x² + 2x + 3 using superscript for powers:
(log-poly (P 'x 2 (N 4) 1 (N 2) 0 3))

; Prints x² - x + 1 omitting one-coefficients.
(log-poly (P 'x 2 (N 1) 1 (I -1) 0 1))

; Prints -4x² - 2x + 3 extracting «-» top omit «+».
(log-poly (P 'x 2 (N -4) 1 -2 0 3))

; Prints 10x² + (y² + 2)x + 3 with polynimial coeffs.
(log-poly (P 'x 2 10 1 (P 'y 2 1 0 2) 0 3))
