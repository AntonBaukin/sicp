(define (log . args) (for-each display args) (newline))

(define eval-amb-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.3.1/eval-impl-amb.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.3.1/eval-impl-forms.scm"
  "../4.3.3/4.3.3-51-pset.scm"
  "../4.3.3/4.3.3-52-if-fail.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.3.1/eval-amb.scm")

; In task «2.2.3/2.2.3-2.40.scm» we've created function
; «prime-sum-pairs()» with close intend.

(define (smallest-divisor n)
 (define (divisor i) (= 0 (remainder n i)))
 (define (step i) (if (= i 2) 3 (+ i 2)))

 (define (next i)
  (if (> (square i) n) n
   (if (divisor i) i (next (step i)))
  )
 )

 (next 2)
)

(define (prime? n)
 (= n (smallest-divisor n))
)

; We define «prime?» as a native function:
(amb-eval-define 'prime? prime?)

; Here we implement «prime-sum-pair()» via amb.
(eval-basic
 (define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items)))
 )

 (define (prime-sum-pair a-list b-list)
  (define a (an-element-of a-list))
  (define b (an-element-of b-list))

  (require (prime? (+ a b)))
  (cons a b)
 )
 
 (global prime-sum-pair)
)

; And what the task' sample is expected to return:
(assert-equal? '((3 . 20) (3 . 110) (8 . 35))
 (eval-amb-results
  (prime-sum-pair
   '(1 3 5 8)
   '(20 35 110)
  )
 )
)

; First, note that «eval-amb-results» returns all the results as a list:
; in our case, this is a single result being the list of the pairs.
;
; This is the same list as collected above via direct call
; of «prime-sum-pair()» with «eval-amb-results».
;
; So, what we achive with the given sample of «permanent-set!»
; side effect combined with «if-fail()» function?
;
; This is inner implementation of the same collect task.
;
(assert-equal? '(((8 . 35) (3 . 110) (3 . 20)))
 (eval-amb-results
  (let ((pairs '()))
   (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
     (permanent-set! pairs (cons p pairs))
     (amb)
    )
    pairs
   )
  )
 )
)
