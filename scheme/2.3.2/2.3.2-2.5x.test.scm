(include "2.3.2-2.5x.scm")
(define (log . args) (for-each display args) (newline))

(log "deriv (+ x 3) = " (deriv-x '(+ x 3)))
(log "deriv (* x y) = " (deriv-x '(* x y)))
(log "deriv (* (* x y) (+ x 3)) = " (deriv-x '(* (* x y) (+ x 3))))
