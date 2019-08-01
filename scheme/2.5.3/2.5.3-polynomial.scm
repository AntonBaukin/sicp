
(define (install-polynomial-package scope)
 (include "../2.3.3/sorted-set.scm")
 (include "2.5.3-polynomial-make.scm")
 (include "2.5.3-polynomial-ops.scm")
 (include "2.5.3-polynomial-str.scm")


 (define TAG  'polynomial)    ;<— polynomial generic type
 (define TTAG 'sparse)        ;<— type for sparse tags set
 (define TAG1 (list TAG))
 (define TAG2 (list TAG TAG))
 (define TTG2 (list TTAG TTAG))
 (define STTG (list 'string TTAG))


 ; Compares terms by their order number.
 ; Here the order is term power descending.
 (define (term<? a b)
  (< (car b) (car a))
 )

 ; We use sorted sets from §2.3.3 for the terms.
 ; (These sets are essentially lists.)
 (define TermsSet (make-sorted-set term<?))

 (define makers (make-polynomial-makers TAG TTAG TermsSet))
 (define make-poly (car makers))
 (define make-poly-from (cadr makers))
 (define reduce-terms (caddr makers))
 (define apply-generic-num (apply-generic-scope-function numbers-scope))
 (define sparse-terms->str (make-polynomial->str))


 ; Formats polynomial to a string by invoking terms
 ; formatter in a general way.
 (define (poly->str poly)
  (apply-generic-num
   'str
   ; Terms formatter takes two arguments:
   ; a string, and general terms object.
   (apply-generic-tag 'string (car poly))
   (cdr poly) ;<— general terms
  )
 )


 (define (call-sparse op a b)
  (num-tag-set TTAG (op a b))
 )

 (define ops (make-polynomial-ops reduce-terms))
 (define add-sparse-terms (curry call-sparse (list-ref ops 0)))
 (define sub-sparse-terms (curry call-sparse (list-ref ops 1)))
 (define mul-sparse-terms (curry call-sparse (list-ref ops 2)))


 ; Checks the same variable of polynomials,
 ; then calls the binary operation on terms
 ; using generic apply from numbers scope.
 (define (same?->call op a b)
  (if (and (symbol? (car a)) (eq? (car a) (car b)))
   (num-tag-set TAG
    (cons (car a) ;<— polynomial's variable
     (num-call op (cdr a) (cdr b)) ;<— resulting general term
    )
   )
   (error "Polynomials with different vars" (car a) (car b))
  )
 )


 ; We assume that '() terms are always zero, and each
 ; general op on a term resulting zero returns it.
 (define (poly-zero? p)
  (null? (cdr p))
 )


 ; Register generic functions:
 ((apply-generic-scope-register scope)
  'zero? TAG1 poly-zero?
  'str   TAG1 poly->str

  ; Here we register ops on general polynomials:
  'add   TAG2 (curry same?->call 'add)
  'sub   TAG2 (curry same?->call 'sub)
  'mul   TAG2 (curry same?->call 'mul)


  ; And here are ops on sparse terms sets:
  'add   TTG2 add-sparse-terms
  'sub   TTG2 sub-sparse-terms
  'mul   TTG2 mul-sparse-terms


  ; Using special '(string sparse) types we register
  ; general formatter for sparse terms set.
  'str   STTG sparse-terms->str
 )


 ; Package exposed items & functions:
 (list make-poly make-poly-from sparse-terms->str)
)
