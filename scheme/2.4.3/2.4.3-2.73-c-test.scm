(include "2.4.3-2.73-c.scm")
(define (log . args) (for-each display args) (newline))

(install-deriv-sum)
(install-deriv-product)
(install-deriv-exp)

(log "deriv (+ x 3) = " (deriv-x '(+ x 3)))
(log "deriv (^ x 3) = " (deriv-x '(^ x 3)))
(log "deriv (^ (+ x 1) 3) = " (deriv-x '(^ (+ x 1) 3)))
(log "deriv (* (^ x 3) (+ x 1)) = " (deriv-x '(* (^ x 3) (+ x 1))))
(log "deriv (^ (^ x 2) 3) = " (deriv-x '(^ (^ x 2) 3)))
