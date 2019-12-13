(include "agenda.scm")
(include "gates.scm")

(define (log . args) (for-each display args) (newline))

; Logical OR a | b may be written so: ¬(a | b) = ¬a & ¬b.
; Hence: a | b ≡ ¬(¬(a | b)) = ¬(¬a & ¬b).
; It has two NOT delays and single AND.
(define (or-gate2 in0 in1 out)
 (define x (make-wire in0))
 (define y (make-wire in1))
 (define z (make-wire out))

 (inverter in0 x)
 (inverter in1 y)
 (inverter z out)

 (probe x "x")
 (probe y "y")
 (probe z "z")
 (and-gate x y z)
)

(define agenda (make-agenda '(2 3 5)))

(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))

(probe a "a")
(probe b "b")
(probe c "c")

(log "a | b ≡ ¬(¬a & ¬b) => c")
(or-gate2 a b c)
(log "Set a = 1")
(set-signal a #t)
(log "Propagate")
(propagate agenda)
(log "time = " (get-time agenda) " is 2 x INV + AND")
