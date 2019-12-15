(include "../3.3.2/assert.scm")
(include "agenda.scm")
(include "gates.scm")
(include "adders.scm")

(define (log . args) (for-each display args) (newline))

(define (log-time agenda . args)
 (apply log (append (list "@" (get-time agenda) " ") args))
)

(define (assert-signal s . wires)
 (for-each (lambda (w) (assert-eq? s (get-signal w))) wires)
)


(define (test immediate? it)
 (define agenda (make-agenda (list 2 3 5 immediate?)))

 (define a (make-wire agenda))
 (define b (make-wire agenda))
 (define c (make-wire agenda))
 (define s (make-wire agenda))
 (define H (half-adder a b s c))
 (define d (list-ref H 2))
 (define e (list-ref H 3))

 (log "Initial probes of " (if immediate? "immediate" "delayed") " wires:")
 
 (probe a "a")
 (probe b "b")
 (probe c "c")
 (probe s "s")
 (probe d "d")
 (probe e "e")

 (it agenda a b c s d e)
)

; Check half adder with default immediate wire action –>
(test #t
 (lambda (agenda a b c s d e)
  (assert-signal #f a b c d e s)
  (log-time agenda "initial propagate:")
  (propagate agenda)
  (assert-signal #f a b c d s)
  (assert-signal #t e)

  (log-time agenda "set a = 1")
  (set-signal a #t)
  (log-time agenda "calc propagate:")
  (propagate agenda)
  (assert-signal #f b c)
  (assert-signal #t a d e s)
 )
)

(newline)

; Check half adder with delayed wire action –>
(test #f
 (lambda (agenda a b c s d e)
  (assert-signal #f a b c d e s)
  (log-time agenda "initial propagate (no trigger):")
  (propagate agenda)
  (assert-signal #f a b c d s)

  ; As wires do notify their listeners on signal change,
  ; gates do not trigger, and inner e-wire of half adder
  ; is not inverted to 1.
  (assert-signal #f e)

  (log-time agenda "set a = 1")
  (set-signal a #t)
  (log-time agenda "calc propagate:")
  (propagate agenda)

  ; When we set a-wire, AND ab-gate does not change, and
  ; e-wire stays 0, but it has to be 1, and output sum
  ; on s-wire is incorrect!
  (assert-signal #f e)

  ; To cope with this we have to notify wire listeners
  ; on each signal set, not alternation.
 )
)
