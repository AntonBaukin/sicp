(include "2.5.3-base.scm")


(log "2x + 3 = " (P 'x 1 2 0 3))

; Prints 4x² + 2x + 3 using superscript for powers:
(log-poly (P 'x 2 (N 4) 1 (N 2) 0 3))

; Prints x² - x + 1 omitting one-coefficients.
(log-poly (P 'x 2 (N 1) 1 (I -1) 0 1))

; Prints -4x² - 2x + 3 extracting «-» top omit «+».
(log-poly (P 'x 2 (N -4) 1 -2 0 3))

; Prints 10x² + (y² + 2)x + 3 with polynimial coeffs.
(log-poly (P 'x 2 10 1 (P 'y 2 1 0 2) 0 3))


; Test add with reduced term:
(log-poly-add
 (P 'x 1 5 0 5)
 (P 'x 1 2 0 -5)
)

(log-poly-add
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 -2 0 1)
)

(log-poly-sub
 (P 'x 100 2 10 2 0 1)
 (P 'x 10 2 0 1)
)

(log-poly-sub
 (P 'x 5 1 0 -1)
 (P 'x 5 1 3 -1)
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 0 5)
)

(log-poly-mul
 (P 'x 3 1 2 -2 0 1)
 (P 'x 0 -1)
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 1 1 0 2)
)

(log-poly-mul
 (P 'x 3 1 2 2 0 1)
 (P 'x 3 1 2 -2 0 -1)
)
