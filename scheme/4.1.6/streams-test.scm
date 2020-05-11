(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")
(include "eval-assert.scm")

; Enable debug mode:
(eval-basic (debug on))

; The tests are from «3.5.1/streams-test.scm».
(include "eval-streams.scm")

; Streams library: delay and promise?
(assert-true?
 (eval-basic
  (define p (delay (+ 1 2 3)))
  (promise? p)
 )
)

; Streams library: delay and force.
(assert-eq? 1
 (eval-basic
  (define p (delay a))
  (define a 1)
  (force p)
 )
)

(assert-eq? 6
 (eval-basic
  (define p (delay (+ a b c)))
  (define a 1)
  (define b 2)
  (define c 3)
  (force p)
 )
)

(eval-basic
 ; Streams library: streams delayed building.
 (define calced-1 0)

 (define (calc-1)
  (set! calced-1 (+ 1 calced-1))
  (cons-stream 1 the-empty-stream)
 )

 (define s01 (cons-stream 0 (calc-1)))
 (assert-eq? 0 calced-1)
 (assert-equal? '(0 1) (stream->list s01))
 (assert-eq? 1 calced-1)

 ; Streams library: force memoization check.
 (assert-equal? '(0 1) (stream->list s01))
 (assert-true? (or (= 1 calced-1) (= 2 calced-1)))

 (debug log "Special form delay is memoized? "
  (if (= 1 calced-1) "yes" "no")
 )

 ; Test stream basics on sample [1; 10] range:
 (define r10 (stream-enumerate-range 1 10))
 (assert-true? (stream-pair? r10))
 (assert-eq? 10 (stream-length r10))
 (assert-equal? '(1 2 3 4 5 6 7 8 9 10) (stream->list r10))

 (define integers (integers-stream 1))
 (assert-equal? '(1 2 3 4 5 6 7 8 9 10) (sub-stream->list 10 integers))

 (define ones (stream-of 1))
 (assert-equal? '(1 1 1 1 1) (sub-stream->list 5 ones))

 (define integers-again (cons-stream 1 (add-streams ones integers-again)))
 (assert-equal? '(1 2 3 4 5 6 7 8 9 10) (sub-stream->list 10 integers-again))

 (define fibs (cons-stream 0 (cons-stream 1 (add-streams fibs (stream-cdr fibs)))))
 (assert-equal? '(0 1 1 2 3 5 8 13 21 34) (sub-stream->list 10 fibs))

 (define integers*2 (scale-stream 2 integers))
 (assert-equal? '(2 4 6 8 10) (sub-stream->list 5 integers*2))

 (assert-equal? '(1 2 3 4 5) (stream->list (list->stream '(1 2 3 4 5))))

 (define integers-even (stream-filter even? integers))
 (assert-equal? '(2 4 6 8 10) (sub-stream->list 5 integers-even))

)

