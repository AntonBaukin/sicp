
(define (make-polynomial-ops reduce-terms)
 (include "2.5.3-iterate-two.scm")

 ; Checks the same variable of polynomials,
 ; then calls the binary operation.
 (define (same?->call op a b)
  (if (and (symbol? (car a)) (eq? (car a) (car b)))
   (cons (car a) (op (cdr a) (cdr b)))
   (error "Polynomials with different vars" (car a) (car b))
  )
 )

 ; General operation on two terms of the same order.
 ; Invoked with coefficients of the terms.
 (define (term-op op a b)
  (cons (car a) (op (cdr a) (cdr b)))
 )

 ; Unlike SICP course, we define linear ops on
 ; terms as special merge on two sorted sets.
 (define (linear-poly-op term-op terms-a terms-b)
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

 (define (same?->linear-call term-op)
  (define poly-call (curry same?->call (curry linear-poly-op term-op)))
  (lambda (a b) (poly-call a b))
 )

 ; As in SICP, we use general arithmetics on terms.
 (define add-term-op (curry term-op add))
 (define add-poly (same?->linear-call add-term-op))

 (define sub-term-op (curry term-op sub))
 (define sub-poly (same?->linear-call sub-term-op))

 (list add-poly sub-poly)
)