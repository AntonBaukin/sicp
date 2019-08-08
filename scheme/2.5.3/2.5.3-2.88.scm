(include "2.5.3-base.scm")


; In this implemetation subtraction of polynomials
; is implemented directly via general sub operation.
;
; By the way, liniear operations, add and sub, were
; generalized in «2.5.3-polynomial-ops.scm».
;
(log-poly-sub
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 2 0 1)
)
