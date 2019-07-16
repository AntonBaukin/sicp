(include "../2.5.2/2.5.2-arithmetics.scm")
(include "2.5.3-polynomial.scm")

(define (log . args) (for-each display args) (newline))

(install-arithmetic-package
 'polynomial-package
 install-polynomial-package
)

(define N make-number) ;<— shortcuts...
(define P (cadr polynomial-package))

(define (log-poly p)
 (log p " = " (num->str p))
)

(log "2x + 3 = " (P 'x 1 2 0 3))
(log-poly (P 'x 2 (N 4) 1 (N 2) 0 (N 3)))