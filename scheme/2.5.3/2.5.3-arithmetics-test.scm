(include "2.5.3-arithmetics.scm")

(define (log . args) (for-each display args) (newline))

(define I make-integer) ;<— shortcuts...
(define N make-number)

(define (test-add a b)
 (log a " + " b " = " (add a b))
)

(log "Direct operations on the same types.")
(test-add (I 1) (I 2))
(test-add (N 1) (N 2))

(newline)
(log "Mixing plain numbers with generic ones.")
(test-add 1 (N 2))

(newline)
(log "Mixing numbers of various generic types (raising).")
(test-add (I 1) (N 2))
(test-add (I 1) 2)

(newline)
(log "And now we turn on the drop...")
(toggle-drop-on!)

(test-add (N 1.5) (N 2))
(test-add (N 1.5) (N 2.5))
(test-add (I 1) 2)

