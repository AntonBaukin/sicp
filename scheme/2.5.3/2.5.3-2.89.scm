(include "2.5.3-base-dense.scm")


; Prints dense form of 6x⁶ + 4x⁴ + 2x² + 1:
(log-poly (D 'x 6 6 4 4 2 2 0 1))

; Prints 10x² + (y² + 2)x + 3 with polynomial coeffs.
; Mixes two representations of polynomials.
(log-poly (P 'x 2 10 1 (D 'y 2 1 0 2) 0 3))
(log-poly (D 'x 2 10 1 (P 'y 2 1 0 2) 0 3))


; Test add with reduced coefficients:
(log-poly-add
 (D 'x 1 5 0 5)
 (D 'x 1 2 0 -5)
)

(log-poly-add
 (D 'x 2 3 1 5 0 1)
 (D 'x 2 -3 1 5 0 -2)
)

(log-poly-add
 (D 'x 100 2 5 2 0 1)
 (D 'x 100 -2 0 1)
 'raw
)

(log-poly-sub
 (D 'x 100 2 5 2 0 1)
 (D 'x 100 2 0 1)
)

(log-poly-mul
 (D 'x 3 1 2 2 0 1)
 (D 'x 0 5)
)

(log-poly-mul
 (D 'x 3 1 2 -2 0 1)
 (D 'x 0 -1)
)

(log-poly-mul
 (D 'x 3 1 2 2 0 1)
 (D 'x 1 1 0 2)
)

(log-poly-mul
 (D 'x 3 1 2 2 0 1)
 (D 'x 3 1 2 -2 0 -1)
)
