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


(define (log-poly-add a b)
 (log "["
  (num->str a) "] + ["
  (num->str b) "] = "
  (num->str (add a b))
 )
)

; Test add with reduced term:
(log-poly-add
 (P 'x 1 5 0 5)
 (P 'x 1 2 0 -5)
)

(log-poly-add
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 -2 0 1)
)


(define (log-poly-sub a b)
 (log "["
  (num->str a) "] - ["
  (num->str b) "] = "
  (num->str (sub a b))
 )
)

(log-poly-sub
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 2 0 1)
)


(define (log-poly-mul a b)
 (log "["
  (num->str a) "] * ["
  (num->str b) "] = "
  (num->str (mul a b))
 )
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 0 5)
)

(log-poly-mul
 (P 'x 3 1 2 -2 0 1)
 (P 'x 0 -1)
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 1 1 0 2)
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 3 1 2 -2 0 -1)
)
