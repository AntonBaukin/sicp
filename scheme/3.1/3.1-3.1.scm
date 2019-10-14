(include "accumulator.scm")
(define (log . args) (for-each display args) (newline))

(define A (make-accumulator 5 +))

(log "A.0 = " (A))
(log "A.1 = " (A 10))
(log "A.2 = " (A 10))
(log "A.. = " ((accumulate-each A 1 2 3 4 5)))


; Might be a handy tool instead of string-append():
(define S (make-concatenator "•"))

(log ((accumulate-each S "Hello," "great" "world!")))
