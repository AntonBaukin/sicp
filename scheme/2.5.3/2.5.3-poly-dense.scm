
; Alternative implementation of polynomials using
; coefficients array (instead of a tree).
; Used in tasks 2.89 and 2.90.
;
; The order of terms in the array is power ascending
; that is opposite to sample given in SICP. Index
; in the array is the term order — not depending
; on the length of the array.
;
; We also implement raise and drop operations to
; sparse tree polynomials. This allows to reuse
; to-string formatter and make functions.
;
; Requires polynomial package to be installed
; and given as the first argument.
;
(define (install-poly-dense-package TTAG polynomial-package scope)
 (define TAG  (list-ref polynomial-package 0))
 (define TAG1 (list TAG))
 (define TAG2 (list TAG TAG))


 ; We reuse various checks and the formatting from
 ; sparse polynomials to simplify the implementation.
 (define make-poly (list-ref polynomial-package 1))
 (define make-poly-from (list-ref polynomial-package 2))
 (define poly->str (list-ref polynomial-package 3))


 ; General polynomial terms are a set sorted in power
 ; descending. Here we convert it to a list of coeffs.
 ; Income terms must be reversed to power ascending,
 ; and the resulting list has power descending order.
 (define (terms->dense-iter terms i res)
  (if (null? terms) res
   (if (= i (caar terms)) ;<— Term order is current?
    ; Here we advance to the next term:
    (terms->dense-iter (cdr terms) (+ i 1)
     (cons (cdar terms) res) ;<— add coeff
    )

    ; If current slot's power is lower, we do not
    ; advance the term, but just add zero coeff.
    (terms->dense-iter terms (+ i 1) (cons 0 res))
   )
  )
 )

 (define (poly-terms->dense terms)
  (reverse (terms->dense-iter (reverse terms) 0 '()))
 )

 ; Takes generic polynomial (not wrapped).
 (define (poly->dense poly)
  (cons (car poly) (poly-terms->dense (cdr poly)))
 )


 ; The following creators are from general polynomials.
 (define (make-poly-dense var terms)
  (num-tag-set TAG (poly->dense
   (apply-generic-unwrap
    (make-poly var terms)
   )
  ))
 )

 (define (make-poly-dense-from var . defs)
  (num-tag-set TAG (poly->dense
   (apply-generic-unwrap
    (apply make-poly-from (cons var defs))
   )
  ))
 )


 ; Back converts a list of dense coeffs to terms.
 ; The resulting list has the required terms order:
 ; term power descending.
 (define (dense->terms-iter coeffs i res)
  (if (null? coeffs) res
   ; Coeff is general zero?
   (if (zero? (car coeffs))
    ; Then just skip this coeff:
    (dense->terms-iter (cdr coeffs) (+ i 1) res)

    ; Create term from i-order and this coeff:
    (dense->terms-iter (cdr coeffs) (+ i 1)
     (cons (cons i (car coeffs)) res)
    )
   )
  )
 )
 

 ; Takes dense polynomial (not wrapped).
 (define (dense->poly dense-poly)
  (cons
   (car dense-poly)
   (dense->terms-iter (cdr dense-poly) 0 '())
  )
 )

 (define (dense-poly->str dense-poly)
  (poly->str (dense->poly dense-poly))
 )

 (define (dense-poly-zero? p)
  (null? (cdr p))
 )
 

 ; Register generic functions:
; ((apply-generic-scope-register scope)
;  'zero? TAG1 dense-poly-zero?
;  'str   TAG1 dense-poly->str
; )


 (list ; Package exposed functions:
  make-poly-dense
  make-poly-dense-from
  dense-poly->str

  ; We may treat convert functions as raise and drop
  ; operations. In the tower generic polynomials are
  ; above the dense ones.

  poly->dense  ; Drops generic polynomial to dense one.
  dense->poly  ; Raises dense polinomial to generic one.
 )
)
