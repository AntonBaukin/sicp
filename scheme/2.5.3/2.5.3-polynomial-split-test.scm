(include "2.5.3-base.scm")
(include "2.5.3-polynomial-split.scm")

(install-arithmetic-package
 'polynomial-split
 install-polynomial-split
)

(define poly-split polynomial-split)

(define (log-poly-split p)
 (log
  (num->str p)
  " ~> "
  (poly-split p)
 )
)

;(log-poly-split (P 'x 2 10 1 (P 'y 2 1 0 2) 0 3))

(log-poly-split (P 'x 2 10 1 (P 'y 2 (P 'z 2 3 1 3) 0 2) 0 3))
