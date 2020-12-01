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

(define (list->stream l)
 (iterator->stream (list-iterator-ext void l))
)

( ; Tests streaming of empty list iterator:
 (lambda ()
  (assert-test (list->stream '()) stream-null?)
 )
)

( ; Tests streaming of single item list iterator:
 (lambda ()
  (define s (list->stream '(a)))
  (assert-eq? 1 (stream-length s))
  (assert-eq? 'a (stream-ref s 0))
 )
)

( ; Tests streaming of several items list iterator:
 (lambda ()
  (define s (list->stream '(a b c d)))

  (assert-eq? 4 (stream-length s))
  (assert-eq? 'a (stream-ref s 0))
  (assert-eq? 'b (stream-ref s 1))
  (assert-eq? 'c (stream-ref s 2))
  (assert-eq? 'd (stream-ref s 3))
 )
)

( ; Tests streaming of several items list iterator:
 (lambda ()
  (define a (list->stream '(a b c d)))
  (define b (list->stream '(1 2 3)))
  (define s (stream-append a b))

  (assert-equal? '(a b c d 1 2 3) (stream->list s))
 )
)

( ; Tests streams interleaved flattening:
 (lambda ()
  (define a (list->stream '(a b)))
  (define b (list->stream '(1 2 3)))
  (define c (list->stream '(- + *)))
  (define d (list->stream '(U W)))
  (define s (list->stream (list a b c d)))

  (assert-equal? '(a 1 b - 2 U 3 + W *) (stream->list (stream-flatten s)))
 )
)

( ; Tests streams (some being nulls) flattening:
 (lambda ()
  (define a (list->stream '(a b)))
  (define b (list->stream '(1 2 3)))
  (define c (list->stream '(c)))
  (define d (list->stream '()))
  (define s (list->stream (list a b d c d)))

  (assert-equal? '(a 1 b c 2 3) (stream->list (stream-flatten s)))
 )
)

( ; Tests stream filter:
 (lambda ()
  (assert-equal? '(2 6 8)
   (stream->list (stream-filter even? (list->stream '(1 2 3 5 6 8 9))))
  )
 )
)

( ; Tests stream filter with transform:
 (lambda ()
  (assert-equal? '(4 36 64)
   (stream->list
    (stream-filter
     (lambda (n)
      (if (even? n) (square n) #f)
     )
     (list->stream '(1 2 3 5 6 8 9))
    )
   )
  )
 )
)
