(include "2.4.3-2.73-ab.scm")
(define (log . args) (for-each display args) (newline))

(install-deriv-sum)
(install-deriv-product)

(log "deriv (+ x 3) = " (deriv-x '(+ x 3)))
(log "deriv (* x y) = " (deriv-x '(* x y)))
(log "deriv (* (* x y) (+ x 3)) = " (deriv-x '(* (* x y) (+ x 3))))
