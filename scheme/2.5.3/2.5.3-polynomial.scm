

(define (install-polynomial-package scope)
 (include "../2.3.3/sorted-set.scm")
 (include "2.5.3-iterate-two.scm")
 (include "superscript-digits.scm")

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
 (define make-terms-set (set-op-make TermsSet))

 ; Returns a wrong term pair, or '().
 (define (check-terms terms)
  (if (null? terms) '()
   (let ((t (car terms)))
    (if
     (and
      (pair? t)
      (integer? (car t))
     )
     (check-terms (cdr terms))
     t ;<— resulting wrong term
    )
   )
  )
 )

 ; Creates polynomial from term pairs.
 ; Does the checks.
 (define (make-poly var terms)
  (cond
   ((not (symbol? var))
    (error "Making with not a polynomial variable symbol" var)
   )

   ((not (null? (check-terms terms)))
    (error "Making polynomial with a wrong term" (check-terms terms))
   )

   (else
    (let ((set (make-terms-set terms)))
     (if (= (length terms) (length set))
      (num-tag-set TAG (cons var set))
      (error "Making polynomial with duplicate term orders" terms)
     )
    )
   )
  )
 )

 (define (defs->terms terms defs)
  (if (null? defs) terms
   (if (null? (cdr defs))
    (error "Making polynomial with not enough definitions" defs)
    (defs->terms
     ; Revers order has no matter due the sorting.
     (cons (cons (car defs) (cadr defs)) terms)
     (cddr defs) ;<— take two items
    )
   )
  )
 )

 ; Creates polynomial from the given variable symbol
 ; and the flat terms: order-i coeff-i ...
 ; Human-friendly, for the tests.
 (define (make-poly-from var . defs)
  (make-poly var (defs->terms '() defs))
 )

 ; Converts coefficient to a string. It has special
 ; trearing for strings ending with period: it cuts
 ; «.» out to make the number prettier.
 (define (coeff->str n)
  (let* (
    (s (num->str n))
    (i (- (string-length s) 1))
    (x (string-ref s i))
   )
   (if (not (eq? x #\.)) s
    (substring s 0 i)
   )
  )
 )

 ; Converts order and term variable to a superscript
 ; power of this variable.
 (define (order->str v o)
  (cond
   ((= 0 o) "")
   ((= 1 o) (symbol->string v))
   (else (string-append
    (symbol->string v)
    (replace-superscript-digits (number->string o))
   ))
  )
 )

 (define (term->str v o c s)
  (let* (
    (cs (coeff->str c))
    ;–> coeff sign integer, leading term has no sign.
    ;(sg (if (= 0 (length s)) 0 (coeff->sign c)))
   )

   (string-append
    (if (= 0 (string-length s )) "" " + ")
    cs
    (order->str v o)
   )
  )
 )

 (define (poly->str-iter var terms str)
  (if (null? terms) str
   (poly->str-iter var (cdr terms)
    (string-append str
     (term->str var (caar terms) (cdar terms) str)
    )
   )
  )
 )

 (define (poly->str poly)
  (poly->str-iter (car poly) (cdr poly) "")
 )

 ; Register generic functions:
 ((apply-generic-scope-register scope)
  'str TAG1 poly->str
 )
 
 (list make-poly make-poly-from)
)
