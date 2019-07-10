(include "2.5.2-arithmetics.scm")
(include "2.5.2-drop.scm")

(define (log . args) (for-each display args) (newline))

; Implements special drop(): it returns '() if drop
; is not possible. Read comments in «2.5.2-drop.scm».
(define drop-impl (install-drop-package numbers-scope))

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


(test-drop (make-integer 2))
(test-drop (make-rat 10 1))
(test-drop (make-rat 10 3))
(test-drop (make-number 3))
(test-drop (make-number (/ 7.0 3)))
(test-drop (make-complex-xy 3.0 0))
(test-drop (make-complex-xy (/ 7.0 5) 0))
(test-drop (make-complex-xy (/ 7.0 5) 1))
