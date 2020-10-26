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

; Test iterator.
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
