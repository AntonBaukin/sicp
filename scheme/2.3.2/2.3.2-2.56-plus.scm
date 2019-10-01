(include "2.3.2-2.5z-plus.scm")
(define (log . args) (for-each display args) (newline))

(log "deriv (- (* 2 x) 3) = " (deriv-x '(- (* 2 x) 3)))
(log "deriv (/ (+ x 1) 2) = " (deriv-x '(/ (+ x 1) 2)))
(log "deriv (/ (+ (^ x 2) 1) 2) = " (deriv-x '(/ (+ (^ x 2) 1) 2)))
(log "deriv (/ (- (^ x 2) (* 2 x)) 2) = " (deriv-x '(/ (- (^ x 2) (* 2 x)) 2)))
