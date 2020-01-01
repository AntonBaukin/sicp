(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))

(define (squarer a b)
 (multiplier a a b)
)

(define a (make-connector))
(define b (make-connector))

(probe "a" a)
(probe "b" b)

(squarer a b)

(connector-set-value a 2 'user)

; Probe [b] ~=> 4
; Probe [a] ~=> 2

; As we see, forward calculation of square
; works prerry well:
(assert-eq? 2 (connector-get-value a))
(assert-eq? 4 (connector-get-value b))

(connector-reset a 'user)
(connector-set-value b 4 'user)

; Probe [b] ~=> 4

; Backward calculation of square root is not possible,
; and connector «a» stays undefined:
(assert-eq? 4 (connector-get-value b))
(assert-true? (not (connector-has-value? a)))

; This is because multiplier may find out only one term,
; having two other both defined.
