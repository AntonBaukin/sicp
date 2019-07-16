(include "2.5.2-arithmetics.scm")
(include "2.5.2-tower.scm")
(include "2.5.2-drop.scm")

(define (log . args) (for-each display args) (newline))

; Implements special drop(): it returns '() if drop
; is not possible. Read comments in «2.5.2-drop.scm».
(define drop-impl
 (install-drop-package
  numbers-scope
  10000
  make-number
  (lambda (n) (< (abs n) 0.0005))
 )
)

; Required version of drop(): it returns the source
; number if the drop is not possible.
(define (drop n)
 (let ((x (drop-impl n)))
  (if (null? x) n x)
 )
)

(define (test-drop n)
 (log "drop " n " —> " (drop n))
)

; These are direct calls for drop().
(test-drop (make-integer 2))
(test-drop (make-rat 10 1))
(test-drop (make-rat 10 3))
(test-drop (make-number 3))
(test-drop (make-number (/ 7.0 3)))
(test-drop (make-complex-xy 3.0 0))
(test-drop (make-complex-xy (/ 7.0 5) 0))
(test-drop (make-complex-xy (/ 7.0 5) 1))


(define (test-add a b)
 (log a " + " b " = " (add a b))
)

(newline)
(log "The following generic call has no the result dropped:")
(test-add (make-complex-xy (/ 7.0 5) 1) (make-complex-xy 0.6 -1))


; Now we overwtite the result post-processing applying drop().
(define (num-call-result result) (drop result))

(newline)
(log "And now we apply drop() to the result:")

; It demonstrates double drop recursion: it reduces the real
; part of a complex number, and that number itself.
(test-add (make-complex-xy (/ 7.0 5) 1) (make-complex-xy 0.6 -1))
