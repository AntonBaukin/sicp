(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))

; Check sum constraints and connector core...
(define a (make-connector))
(define b (make-connector))
(define s (make-connector))

(assert-true? (connector? a))
(assert-true? (not (connector? '(a b c))))

(assert-true? (not (connector-has-value? a)))
(assert-eq? void (connector-get-value a))

(probe "a" a)
(probe "b" b)
(probe "s" s)

(define a+b=s (adder a b s))
(assert-true? (constraint? a+b=s))
(assert-true? (not (connector? a+b=s)))

(assert-true? (not (connector-has-value? a)))
(assert-true? (not (connector-has-value? b)))
(assert-true? (not (connector-has-value? s)))

(connector-set-value a 1 'user)

(assert-true? (connector-has-value? a))
(assert-eq? 1 (connector-get-value a))
(assert-true? (not (connector-has-value? b)))
(assert-true? (not (connector-has-value? s)))

(connector-set-value b 2 'user)

(assert-true? (connector-has-value? a))
(assert-true? (connector-has-value? b))
(assert-true? (connector-has-value? s))

(assert-eq? 1 (connector-get-value a))
(assert-eq? 2 (connector-get-value b))
(assert-eq? 3 (connector-get-value s))

; Note that we have to reset via the same connection
; we previously assigned the value, or our reset call
; is ignored as it has else informant:
(connector-reset s 'user) ;<— ignored

(assert-true? (connector-has-value? a))
(assert-true? (connector-has-value? b))
(assert-true? (connector-has-value? s))

(connector-reset a 'user)

(assert-true? (not (connector-has-value? a)))
(assert-true? (not (connector-has-value? s)))
(assert-eq? void (connector-get-value a))
(assert-eq? void (connector-get-value s))

; User-defined connector «b» value is not reset:
(assert-true? (connector-has-value? b))
(assert-eq? 2 (connector-get-value b))

(connector-set-value a -3 'user)

(assert-true? (connector-has-value? a))
(assert-true? (connector-has-value? b))
(assert-true? (connector-has-value? s))

(assert-eq? -3 (connector-get-value a))
(assert-eq?  2 (connector-get-value b))
(assert-eq? -1 (connector-get-value s))
