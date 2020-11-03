(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "../3.3.2/iterate.scm")
(include "stream.scm")

( ; The following tests streams on delayed building:
 (lambda () ;<— immediately invoked function
  (define calced-1 0)

  (define (calc-1)
   (set! calced-1 (+ 1 calced-1))
   (cons-stream 1 the-empty-stream)
  )

  (define s01 (cons-stream 0 (calc-1)))
  (assert-eq? 0 calced-1)
  (assert-equal? '(0 1) (stream->list s01))
  (assert-eq? 1 calced-1)

  ; The following logs whether delay is memoized:
  (assert-equal? '(0 1) (stream->list s01))
  (assert-true? (or (= 1 calced-1) (= 2 calced-1)))
  (log "Special form delay is memoized? "
   (if (= 1 calced-1) "yes" "no")
  )
 )
)

(define (integers-stream from)
 (cons-stream from (integers-stream (+ 1 from)))
)

; Test sub-stream sampling.
(define integers (integers-stream 1))
(assert-equal? '(1 2 3 4 5) (sub-stream->list 5 integers))

; Tests stream mapping.
(assert-equal? '(1 4 9 16 25)
 (sub-stream->list 5 (stream-map square integers))
)

; Identity function:
(define (identity x) x)

( ; Tests streaming of empty list iterator:
 (lambda () ;<— immediately invoked function
  (define it (list-iterator '()))
  (define s (iterator->stream identity it))
  (assert-test s stream-null?)
 )
)

( ; Tests streaming of single item list iterator:
 (lambda () ;<— immediately invoked function
  (define it (list-iterator '(a)))
  (define s (iterator->stream identity it))

  (assert-eq? 1 (stream-length s))
  (assert-eq? 'a (stream-ref s 0))
 )
)

( ; Tests streaming of several items list iterator:
 (lambda () ;<— immediately invoked function
  (define it (list-iterator '(a b c d)))
  (define s (iterator->stream identity it))

  (assert-eq? 4 (stream-length s))
  (assert-eq? 'a (stream-ref s 0))
  (assert-eq? 'b (stream-ref s 1))
  (assert-eq? 'c (stream-ref s 2))
  (assert-eq? 'd (stream-ref s 3))
 )
)
