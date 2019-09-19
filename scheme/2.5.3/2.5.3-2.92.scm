(include "2.5.3-base.scm")
(include "2.5.3-polynomial-split.scm")
(include "2.5.3-polynomial-group.scm")
(include "2.5.3-polynomial-multi-ops.scm")

(install-arithmetic-package
 'polynomial-split
 install-polynomial-split
)

(install-arithmetic-package
 'polynomial-group
 (curry install-polynomial-group polynomial-package polynomial-split)
)

(define polynomial-multi-ops
 (install-polynomial-multi-ops polynomial-split polynomial-group)
)

(define (log-poly-multi-add . polys)
 (define multi-add (car polynomial-multi-ops))
 
 (define (sum-iter s rest)
  (if (null? rest) s
   (sum-iter
    (string-append s
     (if (= 0 (string-length s)) "" " + ")
     "[" (num->str (car rest)) "]"
    )
    (cdr rest)
   )
  )
 )

 (log
  (sum-iter "" polys) " = "
  (num->str (apply multi-add polys))
 )
)

(define (log-poly-multi-mul poly-a poly-b)
 (define multi-mul (cadr polynomial-multi-ops))
 (log-poly-op "X" multi-mul poly-a poly-b '())
)

; This is a plain addition with the same variable
(log-poly-multi-add ;> 10x³ + 5x² + 2x + 4
 (P 'x 1 2 0 4)
 (P 'x 3 10 2 5)
)

; Now we change the variable and treat y-terms as scalar coefficient:
(log-poly-multi-add ;> 2x + (10y³ + 5y² + 5)
 (P 'x 1 2 0 4)
 (P 'y 3 10 2 5 0 1)
)

; Now we take messed polynomials of two variables:
(log-poly-multi-add ;> (2y + 2)x² + (4y + 3)x + (y² + (4z + 2))
 (P 'x 2 2 1 (P 'y 2 1 1 3 0 3) 0 2)
 (P 'y 2 1 1 (P 'x 2 2 1 1))
)

; And even with three variables of three:
(log-poly-multi-add
 ;> ((2z + 2)y + 2)x² + ((5z + 4)y + (4z + 2))x + ((z + 1)y² + (z²))
 (P 'x 2 2 1 (P 'y 1 3 0 (P 'z 1 4 0 2)))
 (P 'y 2 1 1 (P 'x 2 2 1 1))
 (P 'z 2 1 1 (P 'y 2 1 1 (P 'x 2 2 1 5)))
)

; Multiply of the same variable:
(log-poly-mul
 (P 'x 1 2 0 4)
 (P 'x 2 10 1 5)
)

(log-poly-multi-mul
 (P 'x 1 2 0 4)
 (P 'x 2 10 1 5)
)

; Multiply simply with two variables:
(log-poly-multi-mul
 (P 'x 1 2 0 4)
 (P 'y 2 10 1 5)
)

; Multiply with nested two variables:
(log-poly-multi-mul
 (P 'x 1 (P 'y 1 2 0 2) 0 4)
 (P 'y 2 10 1 5)
)

; And even three variables:
(log-poly-multi-mul
 (P 'x 1 (P 'y 1 2 0 2) 0 4)
 (P 'y 2 10 1 (P 'z 1 2 0 3))
)
