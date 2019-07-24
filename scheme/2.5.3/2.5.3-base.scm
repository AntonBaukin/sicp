(include "2.5.3-arithmetics.scm")
(include "2.5.3-polynomial.scm")


(install-arithmetic-package
 'polynomial-package
 install-polynomial-package
)

(define N make-number)  ;<— shortcuts...
(define I make-integer)
(define P (cadr polynomial-package))


(define (log . args) (for-each display args) (newline))

(define (log-poly p)
 (log p " = " (num->str p))
)

(define (log-poly-add a b)
 (log "["
  (num->str a) "] + ["
  (num->str b) "] = "
  (num->str (add a b))
 )
)

(define (log-poly-sub a b)
 (log "["
  (num->str a) "] - ["
  (num->str b) "] = "
  (num->str (sub a b))
 )
)

(define (log-poly-mul a b)
 (log "["
  (num->str a) "] * ["
  (num->str b) "] = "
  (num->str (mul a b))
 )
)

