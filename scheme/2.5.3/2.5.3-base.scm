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

(define (log-poly-op op-str op a b opts)
 (let ((r (op a b)))
  (log "["
   (num->str a) "] " op-str " ["
   (num->str b) "] = "
   (num->str r)
   (if (null? opts) "" " ")
   (if (equal? '(raw) opts) r "")
  )
 )
)

(define (log-poly-add a b . opts)
 (log-poly-op "+" add a b opts)
)

(define (log-poly-sub a b . opts)
 (log-poly-op "-" sub a b opts)
)

(define (log-poly-mul a b . opts)
 (log-poly-op "*" mul a b opts)
)
