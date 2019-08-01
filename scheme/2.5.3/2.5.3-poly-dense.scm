
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
(define (install-poly-dense-package polynomial-package scope)
 (include "2.5.3-poly-dense-ops.scm")


 (define TAG  'polynomial) ;<— polynomial generic type
 (define TTAG 'sparse)     ;<— generit type of sparse terms set
 (define DTAG 'dense)      ;<— generit type of dense coeffs list
 (define DDG2 (list DTAG DTAG))
 (define SDTG (list 'string DTAG))


 ; We reuse various checks and the formatting from
 ; sparse polynomials to simplify the implementation.
 (define make-poly (list-ref polynomial-package 0))
 (define make-poly-from (list-ref polynomial-package 1))
 (define sparse-terms->str (list-ref polynomial-package 2))


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

 ; Takes wrapped sparse polynomial terms.
 (define (poly-terms->dense terms)
  (let ((set (apply-generic-unwrap terms)))
   (num-tag-set DTAG
    (reverse (terms->dense-iter (reverse set) 0 '()))
   )
  )
 )


 ; Back converts a list of dense coeffs to sparse terms.
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

 ; Takes wrapped dense polynomial list.
 (define (dense->poly-terms coeffs)
  (num-tag-set TTAG
   (dense->terms-iter (apply-generic-unwrap coeffs) 0 '())
  )
 )


 ; The following creators are from sparse polynomials.
 (define (make-poly-dense var terms)
  (let ((p (apply-generic-unwrap (make-poly var terms))))
   (num-tag-set TAG (cons var (poly-terms->dense (cdr p))))
  )
 )

 (define (make-poly-dense-from var . defs)
  (let* (
    (a (cons var defs))
    (p (apply-generic-unwrap (apply make-poly-from a)))
   )
   (num-tag-set TAG (cons var (poly-terms->dense (cdr p))))
  )
 )


 ; Takes variable symbol and unwrapped coeffs list.
 ; Reuses sparse-terms->str() to convert.
 (define (dense-coeffs->str var coeffs)
  (let* (
    (dense  (num-tag-set DTAG coeffs))
    (sparse (dense->poly-terms dense))
    (terms  (apply-generic-unwrap sparse))
   )
   (sparse-terms->str var terms)
  )
 )


 (define (call-dense op a b)
  (num-tag-set DTAG (op a b))
 )

 (define ops (make-poly-dense-ops))
 (define add-dense-terms (curry call-dense (list-ref ops 0)))
 (define sub-dense-terms (curry call-dense (list-ref ops 1)))
 ;(define mul-dense-terms (curry call-dense (list-ref ops 2)))


 ; Register generic functions:
 ((apply-generic-scope-register scope)

  ; Ops on dense terms lists:
  'add DDG2 add-dense-terms
  'sub DDG2 sub-dense-terms
;  'mul   TTG2 mul-dense-terms

  ; Using special '(string dense) types we register
  ; general formatter for dense coeffs list.
  'str SDTG dense-coeffs->str
 )


 ; Package exposed functions:
 (list make-poly-dense make-poly-dense-from)
)
