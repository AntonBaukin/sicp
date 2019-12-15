(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")
(include "../3.1/enumerate.scm")
(include "agenda.scm")
(include "gates.scm")
(include "adders.scm")

(define (log . args) (for-each display args) (newline))

(define (ripple-adder ins0 ins1 carry-in sums carry-out)
 (define n (length sums))

 (define (iter i0 i1 Cin s)
  (define Cout (if (null? (cdr s)) carry-out (make-wire Cin)))

  (full-adder (car i0) (car i1) Cin (car s) Cout)

  (if (not (null? (cdr s)))
   (iter (cdr i0) (cdr i1) Cout (cdr s))
  )
 )

 (assert-true? (> n 0))
 (assert-eq? n (length ins0))
 (assert-eq? n (length ins1))

 (iter ins0 ins1 carry-in sums)
)


(define (make-wires agenda n)
 (map
  (lambda (i) (make-wire agenda))
  (enumerate-n n)
 )
)

(define (signals->str wires)
 (define S (make-concatenator "" signal->str))
 (for-each S wires)
 (S) ;<— returns accumulated string
)

(define (set-signals wires signals)
 (define (next wires chars)
  (if (not (null? wires))
   (begin
    (set-signal (car wires) (eq? #\1 (car chars)))
    (next (cdr wires) (cdr chars))
   )
  )
 )

 (next wires (string->list signals))
)

(define (assert-signals wires signals)
 (assert-equal? signals (signals->str wires))
)

(define (assert-signal s . wires)
 (for-each (lambda (w) (assert-eq? s (get-signal w))) wires)
)


(define agenda (make-agenda '(2 3 5)))

(define a (make-wires agenda 5))
(define b (make-wires agenda 5))
(define i (make-wire agenda))
(define s (make-wires agenda 5))
(define c (make-wire agenda))

(ripple-adder a b i s c)

(define (log-add ci sa sb)
 (set-signal i (= 1 ci))
 (set-signals a sa)
 (set-signals b sb)
 (propagate agenda)

 (log sa " + " sb " + " ci " = "
  (signals->str s) " + " (signal->str c)
 )
)

(log-add 0 "10000" "10000")
(assert-signals s "01000")
(assert-signal #f c)

(log-add 1 "01000" "10000")
(assert-signals s "00100")
(assert-signal #f c)

(log-add 0 "10101" "01010")
(assert-signals s "11111")
(assert-signal #f c)

(log-add 1 "10101" "01010")
(assert-signals s "00000")
(assert-signal #t c)
