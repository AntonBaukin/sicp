
(define (install-polynomial-package scope)
 (include "../2.3.3/sorted-set.scm")
 (include "2.5.3-polynomial-make.scm")
 (include "2.5.3-polynomial-ops.scm")
 (include "2.5.3-polynomial-str.scm")

 (define TAG  'polynomial)
 (define TAG1 (list TAG))
 (define TAG2 (list TAG TAG))

 ; Compares terms by their order number.
 ; (Here the order is descending.)
 (define (term<? a b)
  (< (car b) (car a))
 )

 ; We use sorted sets from §2.3.3 for the terms.
 ; (These sets are essentially lists.)
 (define TermsSet (make-sorted-set term<?))

 (define makers (make-polynomial-makers TAG TermsSet))
 (define make-poly (car makers))
 (define make-poly-from (cadr makers))
 (define reduce-terms (caddr makers))
 (define poly->str (make-polynomial->str))

 (define (call-and-tag op a b)
  (num-tag-set TAG (op a b))
 )
 
 (define ops (make-polynomial-ops reduce-terms))
 (define add-poly (curry call-and-tag (list-ref ops 0)))
 (define sub-poly (curry call-and-tag (list-ref ops 1)))
 (define mul-poly (curry call-and-tag (list-ref ops 2)))

 (define (poly-zero? p)
  (null? (cdr p))
 )

 ; Register generic functions:
 ((apply-generic-scope-register scope)
  'zero? TAG1 poly-zero?
  'str   TAG1 poly->str
  'add   TAG2 add-poly
  'sub   TAG2 sub-poly
  'mul   TAG2 mul-poly
 )
 
 (list make-poly make-poly-from)
)
