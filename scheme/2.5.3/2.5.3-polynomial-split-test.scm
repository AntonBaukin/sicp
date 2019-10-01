(include "2.5.3-base.scm")
(include "2.5.3-polynomial-split.scm")

(install-arithmetic-package
 'polynomial-split
 install-polynomial-split
)

(define poly-split (car polynomial-split))

(define (log-poly-split p)
 (log
  (num->str p)
  " ~> "
  (poly-split p)
  "\n"
 )
)

(log-poly-split (P 'x 3 10 2 5 1 2 0 4))

(log-poly-split (P 'x 2 10 1 (P 'y 2 1 0 2) 0 3))

(log-poly-split (P 'x 2 10 1 (P 'y 2 (P 'z 2 3 1 5) 1 (P 'z 1 7 0 9) 0 2) 0 3))

(log-poly-split (P 'x 2 (P 'y 1 (P 'x 1 2 0 3)) 1 (P 'y 1 (P 'x 2 4 1 5))))

