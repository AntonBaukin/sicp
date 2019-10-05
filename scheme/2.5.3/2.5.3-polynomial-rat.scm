
; Implementation support for GCD and rational cut()
; function for general polynomials. It just unwraps
; two of them and applies generic function for generic
; terms: in our case, various versions for sparse
; terms set (depending on the task).
;
(define (install-polynomial-cut-package polynomial-package scope)
 (define CUT  'cut)
 (define TAG  'polynomial)    ;<— polynomial generic type
 (define TTAG 'sparse)        ;<— type for sparse tags set
 (define TAG2 (list TAG TAG))
 (define TTG2 (list TTAG TTAG))

 (define make-poly (list-ref polynomial-package 0))

 ; Deafult: takes sparse terms and cuts nothing.
 (define terms-cut (lambda (a b) (cons a b)))

 ; Note that the terms list has the power order descending.
 (define (terms-order terms) (caar terms))

 ; Checks the orders of the terms and invokes generic
 ; CUT function with the first term being higher than
 ; the second (but swaps them back in the result).
 ; This makes a favour for the terms GCD.
 (define (terms-cut-safe terms-a terms-b)
  (if (>= (terms-order terms-a) (terms-order terms-b))
   (terms-cut terms-a terms-b)
   (let ((res (terms-cut terms-b terms-a)))
    (cons (cdr res) (car res))
   )
  )
 )

 ; Takes two unwrapped polynomials and delegates cut to the terms.
 (define (poly-cut a b)
  (if (not (and (symbol? (car a)) (eq? (car a) (car b))))
   (error "Can't cut polynomials with different vars" (car a) (car b))
   (let ((terms (num-call CUT (cdr a) (cdr b))))
    (cons
     (make-poly (car a) (car terms))
     (make-poly (car a) (cdr terms))
    )
   )
  )
 )

 ; Delegates cut operation for sparse terms.
 (define (sparse-terms-cut a b) (terms-cut-safe a b))


 ((apply-generic-scope-register scope)
  CUT TAG2 poly-cut
  CUT TTG2 sparse-terms-cut
 )

 ; Returns setter function for internal sparse terms cut:
 (lambda (new-cut) (set! terms-cut new-cut))
)