(include "agenda.scm")
(include "gates.scm")

; See «gates-test.scm» for the tests of logical gates.

(define (log . args) (for-each display args) (newline))

(define agenda (make-agenda '(2 3 5)))

(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))

(probe a "a")
(probe b "b")
(probe c "c")

(log "a | b => c")
(or-gate a b c)
(log "Set a = 1")
(set-signal a #t)
(log "Propagate")
(propagate agenda)

(log "Set a = 0")
(set-signal a #f)
(log "Propagate")
(propagate agenda)
