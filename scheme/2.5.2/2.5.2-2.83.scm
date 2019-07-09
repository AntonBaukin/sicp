(include "2.5.2-arithmetics.scm")
(include "2.5.2-raise.scm")

(define (log . args) (for-each display args) (newline))

(log "raise integer: 1 —> " (raise (make-integer 1)))
(log "raise rational: 1/2 —> " (raise (make-rat 1 2)))
(log "raise float: 2.0 —> " (raise (make-number 2)))
