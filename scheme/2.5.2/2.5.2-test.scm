(include "2.5.2-arithmetics.scm")
(define (log . args) (for-each display args) (newline))

(define (test-add a b)
 (log
  (num->str a) " + "
  (num->str b) " = "
  (num->str (add a b))
 )
)

; Operations on the same types do not require coercion.
(test-add (make-integer 1) (make-integer 2))
(test-add (make-rat 1 2) (make-rat 3 2))
(test-add (make-number 1.2) (make-number 2.8))
(test-add (make-complex-xy 1 0) (make-complex-xy 0 1))

; Uncomment to see error: «Can't coerce 2-arguments integer number»
; (test-add (make-integer 1) (make-number 2))

; Now we include standard coercions forming a tower...
(include "2.5.2-tower.scm")

; Works as we registered special short way omitting rationals.
(test-add (make-integer 1) (make-number 2))

(test-add (make-number 2) (make-complex-xy -1 1))
