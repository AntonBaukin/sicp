(include "2.3.2-2.5z.scm")
(define (log . args) (for-each display args) (newline))

(log "deriv (+ x 3) = " (deriv-x '(+ x 3)))
(log "deriv (^ x 3) = " (deriv-x '(^ x 3)))
(log "deriv (^ (+ x 1) 3) = " (deriv-x '(^ (+ x 1) 3)))
(log "deriv (* (^ x 3) (+ x 1)) = " (deriv-x '(* (^ x 3) (+ x 1))))
(log "deriv (^ (^ x 2) 3) = " (deriv-x '(^ (^ x 2) 3)))
