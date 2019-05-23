(include "2.5.1-arithmetics.scm")
(define (log . args) (for-each display args) (newline))

; We import these internal functions of the complex
; package that do work directly with two internal
; representations of the complex numbers and generic
; functions defined in the local scope.
(define real-part (list-ref complex-package 1))
(define imag-part (list-ref complex-package 2))
(define magnitude (list-ref complex-package 3))
(define angle     (list-ref complex-package 4))

(define pi 3.14159265359)

; These are internal representations of complex numbers in
; the private package — the functions above work with them.
(define int_xy1_1 '(rectangular . (1 . 1)))
(define int_sq2_45 (cons 'polar (cons (sqrt 2) (* 0.25 pi))))

; Support printing functions.
(define (n->s n) (number->string (* 0.001 (round (* n 1000)))))
(define (degrees-str radians)
 (string-append (n->s (/ (* 180.0 radians) pi)) "°")
)

(log "Complex (x: 1 y: 1)"
 " real " (n->s (real-part int_xy1_1))
 " imagine " (n->s (imag-part int_xy1_1))
 " magnitude " (n->s (magnitude int_xy1_1))
 " angle " (degrees-str (angle int_xy1_1))
)

(log "Complex (r: sqrt(2) angle: 45°)"
 " real " (n->s (real-part int_sq2_45))
 " imagine " (n->s (imag-part int_sq2_45))
 " magnitude " (n->s (magnitude int_sq2_45))
 " angle " (degrees-str (angle int_sq2_45))
)

; Now we create complex number from arithmetics package.
(define xy1_1 (make-complex-xy 1 1))
(log "Complex " (num->str xy1_1) " is " xy1_1)

; If we try to apply the following function it will not work
; as xy1_1 is double-tagged with 'complex and 'rectangular,
; but real-part() expects only single wrapping.
; (real-part xy1_1)

((apply-generic-scope-register numbers-scope)
  'real-part '(complex) real-part
  'imag-part '(complex) imag-part
  'magnitude '(complex) magnitude
  'angle     '(complex) angle
)

; Now we define magnitude as a general call. It works so:
;
; 1) general call 'magnitude on numbers-scope removes
; 'complex tag and calls real-part() on internal number
; of the complex package;
;
; 2) that number is tagged as 'rectangular, and real-part()
; is just an alias for generic call in provate complex-scope.
(define magnitude (curry num-call 'magnitude))

(log "Complex " (num->str xy1_1) " magnitude " (n->s (magnitude xy1_1)))
