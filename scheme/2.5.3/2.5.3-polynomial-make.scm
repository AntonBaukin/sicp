
(define (make-polynomial-makers TAG TermsSet)
 (define make-terms-set (set-op-make TermsSet))

 (define (reduce-terms-iter res terms)
  (if (null? terms) res
   ; Coefficient value is zero?
   (if (zero? (cdar terms))
    (reduce-terms-iter res (cdr terms))
    (reduce-terms-iter (cons (car terms) res) (cdr terms))
   )
  )
 )

 ; Removes terms with zero coefficients preserving the order.
 (define (reduce-terms terms)
  (reverse (reduce-terms-iter '() terms))
 )

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
    (let* (
      (xterms (reduce-terms terms))
      (set (make-terms-set xterms))
     )

     (if (= (length xterms) (length set))
      (num-tag-set TAG (cons var set))
      (error "Making polynomial with duplicate term orders" xterms)
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
     ; Reverse order has no matter due the sorting.
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

 (list make-poly make-poly-from reduce-terms)
)