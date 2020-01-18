(include "../3.3.2/assert.scm")
(include "stream.scm")
(include "streams.scm")

(define (log . args) (for-each display args) (newline))


; The following tests streams on delayed building:
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

; Test stream basics on sample [1; 10] range:
(define r10 (stream-enumerate-range 1 10))
(assert-true? (stream-pair? r10))
(assert-eq? 10 (stream-length r10))
(assert-equal? '(1 2 3 4 5 6 7 8 9 10) (stream->list r10))

(define is (integers-stream 1))
(assert-equal? '(1 2 3 4 5 6 7 8 9 10) (sub-stream->list 10 is))

(define ones (stream-of 1))
(assert-equal? '(1 1 1 1 1) (sub-stream->list 5 ones))

(define isAgain (cons-stream 1 (add-streams ones isAgain)))
(assert-equal? '(1 2 3 4 5 6 7 8 9 10) (sub-stream->list 10 isAgain))

(define fibs (cons-stream 0 (cons-stream 1 (add-streams fibs (stream-cdr fibs)))))
(assert-equal? '(0 1 1 2 3 5 8 13 21 34) (sub-stream->list 10 fibs))

(define is2 (scale-stream 2 is))
(assert-equal? '(2 4 6 8 10) (sub-stream->list 5 is2))

(assert-equal? '(1 2 3 4 5) (stream->list (list->stream '(1 2 3 4 5))))