(include "../3.3.2/assert.scm")
(include "agenda.scm")
(include "gates.scm")
(include "adders.scm")

(define (log . args) (for-each display args) (newline))

(define (assert-signal s . wires)
 (for-each (lambda (w) (assert-eq? s (get-signal w))) wires)
)


(define agenda (make-agenda '(2 3 5)))

; Test half adder –>
(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))
(define s (make-wire agenda))
(define t 0)

(half-adder a b s c)
(assert-signal #f a b c s)

(set-signal a #t)
(set! t (get-time agenda))
(propagate agenda)
(assert-signal #f b c)
(assert-signal #t a s)

; Half adder time = max(OR, AND + NOT) + AND
; Here: max(5, 3 + 2) + 3 = 8
(log "Half adder time: " (- (get-time agenda) t))
(assert-eq? 8 (- (get-time agenda) t))

(set-signal b #t)
(propagate agenda)
(assert-signal #f s)
(assert-signal #t a b c)


; Test full adder –>
(define a (make-wire agenda))
(define b (make-wire agenda))
(define c (make-wire agenda))
(define s (make-wire agenda))
(define i (make-wire agenda))

(full-adder a b i s c)
(assert-signal #f a b i s c)

(set-signal a #t)
(propagate agenda)
(assert-signal #f i b c)
(assert-signal #t a s)

(set-signal i #t)
(propagate agenda)
(assert-signal #f b s)
(assert-signal #t a i c)

; Reset full adder before the max time test:
(set-signal a #f)
(set-signal i #f)
(propagate agenda)

(set-signal a #t)
(set-signal b #t)
(set-signal i #t)
(set! t (get-time agenda))
(propagate agenda)
(assert-signal #t a b i s c)

; Full adder max time is: 2 x Half-Adder.
; Here: 2*8 = 16
(log "Full adder time: " (- (get-time agenda) t))
(assert-eq? 16 (- (get-time agenda) t))
