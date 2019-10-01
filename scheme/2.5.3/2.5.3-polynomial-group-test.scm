(include "2.5.3-base.scm")
(include "2.5.3-polynomial-split.scm")
(include "2.5.3-polynomial-group.scm")

(install-arithmetic-package
 'polynomial-split
 install-polynomial-split
)

(install-arithmetic-package
 'polynomial-group
 (curry install-polynomial-group polynomial-package polynomial-split)
)

(define poly-split (car polynomial-split))
(define poly-group (car polynomial-group))
(define collect-vars (cadr polynomial-group))

;(define (log-omap omap)
; ((tree-op-iter IndexTree)
;  ((caddr omap)) ;<— index tree instance
;  (lambda (oes)
;   (log (car oes) ": " (cdr oes))
;   void
;  )
; )
; (newline)
;)

(define (log-poly-group p)
 (let ((s (poly-split p)))
  (log (num->str p) " ~>\n" s)
  (log (num->str (poly-group (collect-vars s) s)))
  (newline)
 )
)

(log-poly-group (P 'x 3 10 2 5 1 2 0 4))

(log-poly-group (P 'x 2 10 1 (P 'y 2 1 0 (P 'x 1 2)) 0 3))

(log-poly-group (P 'x 2 (P 'y 1 (P 'x 1 2 0 3)) 1 (P 'y 1 (P 'x 2 4 1 5))))
