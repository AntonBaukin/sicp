(include "../3.3.2/assert.scm")
(include "agenda.scm")

(define (log . args) (for-each display args) (newline))

(define agenda (make-agenda '(2 3 5)))

(define wire (make-wire agenda))
(define wire-set? void)

(on-wire
 wire
 (lambda (w)
  (assert-eq? w wire)
  (if (eq? void wire-set?)
   ; Invoked immediately on wire listen:
   (begin
    (set! wire-set? #f)
    (assert-eq? #f (get-signal w))
   )

   ; Invoked on each signal alter:
   (begin
    (assert-eq? #f wire-set?)
    (assert-eq? #t (get-signal w))
    (set! wire-set? #t)
   )
  )
 )
)

(assert-eq? #f (get-signal wire))
(set-signal wire #f)
(assert-eq? #f (get-signal wire))
(assert-eq? #f wire-set?)
(set-signal wire #t)
(assert-eq? #t (get-signal wire))
(assert-eq? #t wire-set?)

(assert-eq? 0 (get-time agenda))
(assert-eq? 0 (get-time wire))

(define stage 0)

(define (stage-tester t s)
 (after-delay agenda t
  (lambda (a)
   (assert-eq? agenda a)
   (assert-eq? s stage)
   (assert-eq? t (get-time agenda))
   (set! stage (+ s 1))
  )
 )
)

(stage-tester 2 4) ; t = 2, stage = 4

(stage-tester 1 2) ; t = 1, stage = 2
(stage-tester 1 3) ; t = 1, stage = 3

(stage-tester 2 5) ; t = 2, stage = 5

(stage-tester 0 0) ; t = 0, stage = 0
(stage-tester 0 1) ; t = 0, stage = 1

(assert-eq? 0 stage)
(propagate agenda)
(assert-eq? 2 (get-time agenda))
(assert-eq? 6 stage)
