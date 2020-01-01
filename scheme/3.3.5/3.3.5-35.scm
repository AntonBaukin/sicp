(include "../3.3.2/assert.scm")
(include "connector.scm")
(include "constraints.scm")

(define (log . args) (for-each display args) (newline))


(define (squarer x s)
 (define me (list 'squarer))

 (define (set connector value)
  (connector-set-value connector value me)
 )

 (define (on-value c)
  (cond
   ((connector-has-value? x)
    (set s (square (connector-get-value x)))
   )

   ((connector-has-value? s)
    (set x (sqrt (connector-get-value s)))
   )
  )
 )

 (define (on-reset c)
  (reset-connectors me x s)
 )

 (set-cdr! me (list on-value on-reset x s))
 (connect-each me x s)

 me ;<— resulting instance
)


(define x (make-connector))
(define s (make-connector))

(probe "x" x)
(probe "s" s)

(squarer x s)

(connector-set-value x 2 'user)

; Probe [s] ~=> 4
; Probe [x] ~=> 2

(assert-eq? 2 (connector-get-value x))
(assert-eq? 4 (connector-get-value s))

(connector-reset x 'user)
(connector-set-value s 9 'user)

; And now (see task 34) it's able to find square root:
(assert-eq? 3 (connector-get-value x))
(assert-eq? 9 (connector-get-value s))
