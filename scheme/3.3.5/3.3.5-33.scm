(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))

(define (averager a b avg)
 (define s (make-connector))
 (define sum (adder a b s))
 (define c (make-connector))
 (define C (constant 0.5 c))
 (define mul (multiplier s c avg))

 (list
  'averager
  constraint-noop
  constraint-noop
  a b avg
  s sum
  c C
  mul
 )
)


(define a (make-connector))
(define b (make-connector))
(define avg (make-connector))

(probe "a" a)
(probe "b" b)
(probe "avg" avg)

(averager a b avg)

(connector-set-value a 1 'user)
(connector-set-value b 3 'user)

(assert-eq? 1 (connector-get-value a))
(assert-eq? 3 (connector-get-value b))
(assert-equal? 2. (connector-get-value avg))

(connector-reset a 'user)
(connector-set-value avg 5 'user)

(assert-equal? 7. (connector-get-value a))
(assert-eq? 3 (connector-get-value b))
(assert-eq? 5 (connector-get-value avg))
