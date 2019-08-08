(include "2.5.3-base.scm")


; This is the sample from 2.91 task. Below we check
; that the result is correct. The implementation is
; for sparse polynomials only as it's an effective
; way for the division.
(log-poly-div
 (P 'x 5 1 0 -1)
 (P 'x 2 1 0 -1)
)
 
(log-poly-mul
 (P 'x 3 1 1 1)
 (P 'x 2 1 0 -1)
)

(log-poly-add
 (P 'x 5 1 1 -1)
 (P 'x 1 1 0 -1)
)
