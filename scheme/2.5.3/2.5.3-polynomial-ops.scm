
(define (make-polynomial-ops reduce-terms)
 (include "2.5.3-iterate-two.scm")

 
 ; General operation on two terms of the same order.
 ; Invoked with coefficients of the terms.
 (define (term-op op a b)
  (cons (car a) (op (cdr a) (cdr b)))
 )

 ; Unlike SICP course, we define linear ops on
 ; terms as special merge on two sorted sets.
 (define (linear-terms-op term-op terms-a terms-b)
  (reduce-terms
   (merge-sorted terms-a terms-b (lambda (a b)
    (cond
     ((< (car a) (car b)) #t)
     ((< (car b) (car a)) #f)
     (else (term-op a b))
    )
   ))
  )
 )

 (define (bind-linear-call term-op)
  (define terms-call (curry linear-terms-op term-op))
  (lambda (a b) (terms-call a b))
 )

 ; As in SICP, we use general arithmetics on terms.
 (define add-term-op (curry term-op add))
 (define add-sparse-terms (bind-linear-call add-term-op))
 (define sub-term-op (curry term-op sub))
 (define sub-sparse-terms (bind-linear-call sub-term-op))


 (define (mul-terms a b)
  (cons
   (+ (car a) (car b))   ;<— sum the orders
   (mul (cdr a) (cdr b)) ;<— general mul of the coefficients
  )
 )

 ; Mutiplies terms list by single term reversing the order.
 (define (mul-terms-by-term-iter terms term res)
  (if (null? terms) res
   (mul-terms-by-term-iter (cdr terms) term
    (cons (mul-terms (car terms) term) res)
   )
  )
 )

 (define (mul-poly-terms-iter res terms-a terms-b)
  (if (null? terms-b) res
   (mul-poly-terms-iter
    (linear-terms-op add-term-op res 
     (reverse (mul-terms-by-term-iter terms-a (car terms-b) '()))
    )
    terms-a (cdr terms-b)
   )
  )
 )

 (define mul-sparse-terms (curry mul-poly-terms-iter '()))


 ; Resulting functions:
 (list add-sparse-terms sub-sparse-terms mul-sparse-terms)
)