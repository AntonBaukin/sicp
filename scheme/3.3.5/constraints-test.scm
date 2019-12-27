(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))

(log "Check adder constraint and connector core...")

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


(newline)
(log "Check multiplier constraint...")

(define a (make-connector))
(define b (make-connector))
(define s (make-connector))

(probe "a" a)
(probe "b" b)
(probe "s" s)

(multiplier a b s)

(connector-set-value a 2 'user)

(assert-eq? 2 (connector-get-value a))
(assert-true? (not (connector-has-value? b)))
(assert-true? (not (connector-has-value? s)))

(connector-set-value b 5 'user)

(assert-eq?  2 (connector-get-value a))
(assert-eq?  5 (connector-get-value b))
(assert-eq? 10 (connector-get-value s))

(connector-reset a 'user)

(assert-true? (not (connector-has-value? a)))
(assert-true? (not (connector-has-value? s)))
(assert-eq? 5 (connector-get-value b))

(connector-reset b 'user)
(connector-set-value b 0 'user)

(assert-true? (not (connector-has-value? a)))
(assert-eq? 0 (connector-get-value b))
(assert-eq? 0 (connector-get-value s))

(connector-set-value a 1 'user)

(assert-eq? 1 (connector-get-value a))
(assert-eq? 0 (connector-get-value b))
(assert-eq? 0 (connector-get-value s))

(reset-connectors 'user a b s)

(connector-set-value b  2 'user)
(connector-set-value s 10 'user)

(assert-eq?  5 (connector-get-value a))
(assert-eq?  2 (connector-get-value b))
(assert-eq? 10 (connector-get-value s))


(newline)
(log "Check constant constraint...")

(define a (make-connector))
(define b (make-connector))
(define s (make-connector))

(probe "a" a)
(probe "b" b)
(probe "s" s)

(define C (constant 5 b))

(adder a b s)

(assert-true? (not (connector-has-value? a)))
(assert-true? (not (connector-has-value? s)))
(assert-eq? 5 (connector-get-value b))

; The following reset is ingored:
(connector-reset b 'user)
(assert-eq? 5 (connector-get-value b))

(assert-error
 (lambda () (connector-reset b C))
 (lambda (msg params)
  (assert-equal? "Constant connector may not be altered!" msg)
  (connector-set-value b 5 C) ;<— some type of a hack!
 )
)

(connector-set-value a 2 'user)

(assert-eq? 2 (connector-get-value a))
(assert-eq? 5 (connector-get-value b))
(assert-eq? 7 (connector-get-value s))

(connector-reset a 'user)
(connector-set-value s 4 'user)

(assert-eq? -1 (connector-get-value a))
(assert-eq?  5 (connector-get-value b))
(assert-eq?  4 (connector-get-value s))
