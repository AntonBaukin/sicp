(include "2.4.3-2.73-c-plus.scm")
(define (log . args) (for-each display args) (newline))

(install-deriv-sum)
(install-deriv-product)
(install-deriv-exp)
(install-deriv-diff)
(install-deriv-div)

(log "deriv (- (* 2 x) 3) = " (deriv-x '(- (* 2 x) 3)))
(log "deriv (/ (+ x 1) 2) = " (deriv-x '(/ (+ x 1) 2)))
(log "deriv (/ (+ (^ x 2) 1) 2) = " (deriv-x '(/ (+ (^ x 2) 1) 2)))
(log "deriv (/ (- (^ x 2) (* 2 x)) 2) = " (deriv-x '(/ (- (^ x 2) (* 2 x)) 2)))
