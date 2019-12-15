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


(define (test queue? it)
 (define agenda (make-agenda (list 2 3 5 #t queue?)))

 (define a (make-wire agenda))
 (define b (make-wire agenda))
 (define c (make-wire agenda))
 (define s (make-wire agenda))
 (define H (half-adder a b s c))
 (define d (list-ref H 2))
 (define e (list-ref H 3))

 (log "Testing with " (if queue? "fifo/queue" "lifo-stack") " segments:")

 (probe a "a")
 (probe b "b")
 (probe c "c")
 (probe s "s")
 (probe d "d")
 (probe e "e")

 (it agenda a b c s d e)
)


; Check AND with default queued segment actions –>
(test #t
 (lambda (agenda a b c s d e)
  ; We watch after d-e-s AND gate of half-adder;
  ; e-wire is inverted, thus d = 0, e = 1

  (log-time agenda "initial propagate:")
  (propagate agenda)
  (assert-signal #t e)
  (assert-signal #f a b c d s)

  ; We set a = 1, b = 1
  (log-time agenda "set a = 1, b = 1")
  (set-signal a #t)
  (set-signal b #t)
  (propagate agenda)

  ; The probes do show the following trace log:
  ; [a] @5 ~> 1
  ; [b] @5 ~> 1
  ; [c] @8 ~> 1
  ; [d] @10 ~> 1  <— d-e wires do alter at the same time
  ; [e] @10 ~> 0  <— as it requested in the task,
  ; [s] @13 ~> 1  <— output sum twitches!
  ; [s] @13 ~> 0
  (assert-signal #t a b d c)
  (assert-signal #f e s)
 )
)

(newline)

; Check AND with stacked segment actions –>
(test #f
 (lambda (agenda a b c s d e)
  ; At the start we have no difference:

  (log-time agenda "initial propagate:")
  (propagate agenda)
  (assert-signal #t e)
  (assert-signal #f a b c d s)

  ; We set a = 1, b = 1
  (log-time agenda "set a = 1, b = 1")
  (set-signal a #t)
  (set-signal b #t)
  (propagate agenda)

  ; The probes do show the following trace log:
  ; [a] @5 ~> 1
  ; [b] @5 ~> 1
  ; [c] @8 ~> 1   <— carry bit twitches to wrong!
  ; [c] @8 ~> 0
  ; [e] @10 ~> 0  <— d-e wires do alter at the same time,
  ; [d] @10 ~> 1  <— but in reverse order, and
  ;               <— output sum stays flat!

  (assert-signal #t a b d)
  (assert-signal #f e s c) ; c-wire is wrong!

  ; The problem is in AND-gate implementation.
  ; The signal to assign in the segment action
  ; is fixed before invoking after-delay, but
  ; actual state of the input wires changes
  ; before invoking segment callback due
  ; to callback of sibling input wire
  ; being invoked before.
 )
)

