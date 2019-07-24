(include "2.5.3-base.scm")
(include "2.5.3-poly-dense.scm")


; Install alternate implementation for polinomials.
; We do not use double-tagging as SICP proposed for
; this implementation: as for complex numbers.
(install-arithmetic-package
 'poly-dense-package
 (curry install-poly-dense-package polynomial-package)
)

; Shortcut for from-maker of dense polynomial.
(define D (cadr poly-dense-package))
