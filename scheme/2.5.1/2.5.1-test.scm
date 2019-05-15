(include "2.5.1-arithmetics.scm")
(define (log . args) (for-each display args) (newline))

(log "1/3 + 4/5 = " (num->str (add (make-rat 1 3) (make-rat 4 5))))
