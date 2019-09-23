
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
 (define (sparse-terms-cut a b) (terms-cut a b))


 ((apply-generic-scope-register scope)
  CUT TAG2 poly-cut
  CUT TTG2 sparse-terms-cut
 )

 ; Returns setter function for internal sparse terms cut:
 (lambda (new-cut) (set! terms-cut new-cut))
)