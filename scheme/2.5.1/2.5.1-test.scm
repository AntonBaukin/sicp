(include "2.5.1-arithmetics.scm")
(define (log . args) (for-each display args) (newline))

(define r1_3 (make-rat 1 3))
(define r4_5 (make-rat 4 5))

(define (test-op->str op a b)
 (string-append
  (num->str a) " "
  (num-op->str op) " "
  (num->str b) " = "
  (num->str
   (if (symbol? op)
    (num-call op a b)
    (op a b)
   )
  )
 )
)

;(log "Rational " (num->str r1_3) " is " r1_3)
;(log (test-op->str 'add r1_3 r4_5))
;(log (test-op->str 'sub r4_5 r1_3))

(define pi 3.14159265359)

(define xy1_1 (make-complex-xy 1 1))
(define ra_sq2_45 (make-complex-ra (sqrt 2) (* 0.25 pi)))
(define xy1_0 (make-complex-xy 1 0))
(define xy0_1 (make-complex-xy 0 1))

(log "Complex " (num->str xy1_1) " is " xy1_1)
(log "Complex " (num->str ra_sq2_45) " is " ra_sq2_45)
(log (test-op->str 'add xy1_1 xy1_0))
(log (test-op->str 'sub xy1_1 xy1_0))
(log (test-op->str 'mul xy1_1 xy0_1))
(log (test-op->str 'mul xy0_1 xy0_1))

