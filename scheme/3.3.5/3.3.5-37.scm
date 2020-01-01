(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))


(define (cv v)
 (define c (make-connector))
 (constant v c)
 c ;<— resulting connector
)

(define (c+ a b)
 (define s (make-connector))
 (adder a b s)
 s ;<— resulting connector
)

(define (c- a b)
 (define s (make-connector))
 (define n (make-connector))

 ; Negate «b» value as: n + b = 0
 (adder n b (cv 0))

 (adder a n s)
 s ;<— resulting connector
)

; Test subtractor...
(define a (make-connector))
(define b (make-connector))
(define c (c- a b))

(connector-set-value a 10 'user)
(connector-set-value b 2 'user)
(assert-equal? 8 (connector-get-value c))


(define (c* a b)
 (define s (make-connector))
 (multiplier a b s)
 s ;<— resulting connector
)

(define (c/ a b)
 (define s (make-connector))
 (define i (make-connector))

 ; Invert «b» value as: i * b = 1
 (multiplier i b (cv 1.))

 (multiplier a i s)
 s ;<— resulting connector
)

; Test divider...
(define a (make-connector))
(define b (make-connector))
(define c (c/ a b))

(connector-set-value a 10 'user)
(connector-set-value b 2 'user)
(assert-equal? 5. (connector-get-value c))


(define (celsius-fahrenheit-converter x)
 (c+
  (c*
   (c/ (cv 9) (cv 5))
   x
  )
  (cv 32)
 )
)

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(connector-set-value F 451 'user)
(log (connector-get-value F) "°F = " (connector-get-value C) "°C")
