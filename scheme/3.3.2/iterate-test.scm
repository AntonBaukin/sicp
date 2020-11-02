(define (log . args) (for-each display args) (newline))

(include "assert.scm")
(include "iterate.scm")

(define sample '(1 2 3 4 5))

(define (make-summer)
 (define sum 0)

 (lambda (n)
  (if (eq? '? n)
   sum
   (begin
    (assert-true? (number? n))
    (set! sum (+ sum n))
   )
  )
 )
)

; Test visitor of all items.
(
  (lambda () ;<— immediately invoked function
   (define summer (make-summer))
   (iterate-list sample summer)
   (assert-eq? 15 (summer '?))
  )
)

; Test list iterator.
(
  (lambda () ;<— immediately invoked function
   (define it (list-iterator sample))
   (assert-eq? 1 (it))
   (assert-eq? 2 (it))
   (assert-eq? 3 (it))
   (assert-eq? 4 (it))
   (assert-eq? 5 (it))
   (assert-eq? '() (it))
  )
)

(define (make-finder what)
 (lambda (x)
  (if (equal? what x) #f)
 )
)

(define (find what)
 (iterate-list sample (make-finder what))
)

; Test visitor with find behaviour:
(assert-eq? 1 (find 1))
(assert-eq? 2 (find 2))
(assert-eq? 3 (find 3))
(assert-eq? 4 (find 4))
(assert-eq? 5 (find 5))
(assert-eq? void (find 6))

(define (check-transform source expected transform)
 (define it (list-iterator source))
 (define tit (it-transform it transform))
 (assert-equal? expected (iterator->list tit))
)

; Test transformation of null list.
(check-transform '() '() square)

; Test transformation of single item.
(check-transform '(2) '(4) square)

; Test transformation of the sample.
(check-transform sample '(1 4 9 16 25) square)

(define (check-join expected . lists)
 (define it (list-iterator lists))
 (define jit (join-iterators it list-iterator))
 (assert-equal? expected (iterator->list jit))
)

; Test join iteration of 1..3 empty lists.
(check-join '() '())
(check-join '() '() '())
(check-join '() '() '() '())

; Test join iteration of various combinations.
(check-join '(1) '(1))
(check-join '(1 2) '(1 2))
(check-join '(1 2 3) '(1 2 3))
(check-join '(1 2) '(1) '(2))
(check-join '(1 2 3) '(1) '(2) '(3))
(check-join '(1 2 3) '(1 2) '(3))
(check-join '(1 2 3) '(1) '(2 3))
(check-join '(1 2 3 4 5) '(1) '(2 3) '(4 5))
