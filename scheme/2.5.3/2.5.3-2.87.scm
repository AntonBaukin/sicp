(include "2.5.3-base.scm")


; Prints 10x² + (y² + 2)x + 3 with polynimial coeffs.
;
; Making polynomial with poly-coefficient requires
; testing zero? as wgile making we reduce zero terms.
; In SICP course the same check goes while doing
; arithmetic operations: adjoin-term().
;
; See «2.5.3-polynomial.scm» for zero? predicate.
;
(log-poly (P 'x 2 10 1 (P 'y 2 1 0 2) 0 3))

; This sum has zero terms removed:
(log-poly-add
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 -2 0 1)
)
