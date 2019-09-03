(include "2.5.3-base.scm")
(include "2.5.3-polynomial-split.scm")
(include "2.5.3-polynomial-group.scm")
(include "../2.3.4/tree-iter.scm")

(install-arithmetic-package
 'polynomial-split
 install-polynomial-split
)

(install-arithmetic-package
 'polynomial-group
 install-polynomial-group
)

(define poly-split (car polynomial-split))
(define poly-group polynomial-group)

(define omap-iter (make-tree-order-iter IndexTree))

(define (log-omap omap)
 (omap-iter ((caddr omap))
  (lambda (oes)
   (log (car oes) ": " (cdr oes))
   void
  )
 )
)

(define (log-poly-split p)
 (log
  (num->str p) " ~>\n"
  (poly-split p)
 )

 (log-omap (poly-group '(x) (poly-split p)))
)

(log-poly-split (P 'x 3 10 2 5 1 2 0 4))
