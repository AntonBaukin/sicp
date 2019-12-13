(include "../3.3.2/assert.scm")
(include "agenda.scm")
(include "gates.scm")

(define (log . args) (for-each display args) (newline))

(define (assert-signal s . wires)
 (for-each (lambda (w) (assert-eq? s (get-signal w))) wires)
)

(define agenda (make-agenda '(2 3 5)))

; Test inverter –>
(define a (make-wire agenda))
(define b (make-wire agenda))

(inverter a b)
(assert-signal #f a b)
(propagate agenda)
(assert-signal #f a)
(assert-signal #t b)


; Test and gate –>
(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))

(and-gate a b c)
(assert-signal #f a b c)
(propagate agenda)
(assert-signal #f a b c)
(set-signal a #t)
(propagate agenda)
(assert-signal #t a)
(assert-signal #f b c)
(set-signal b #t)
(propagate agenda)
(assert-signal #t a b c)


; Test or gate –>
(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))

(or-gate a b c)
(assert-signal #f a b c)
(propagate agenda)
(assert-signal #f a b c)
(set-signal a #t)
(propagate agenda)
(assert-signal #t a c)
(assert-signal #f b)
(set-signal b #t)
(propagate agenda)
(assert-signal #t a b c)
