
; Operations for polynomials with dense coeffs lists.
(define (make-poly-dense-ops)
 (include "2.5.3-iterate-two.scm")
 

 ; Takes coeffs in reverse order (power descending to 0)
 ; and returns list in normal order having the higher
 ; zeros trimmed.
 (define (reduce-coeffs-iter coeffs res)
  (if (null? coeffs) res
   (if (zero? (car coeffs))
    (reduce-coeffs-iter (cdr coeffs) res)
    (append res (reverse coeffs)) ;<— break iteration
   )
  )
 )

 ; Takes two lists of coefficients and merges
 ; them with the operation given.
 (define (linear-coeffs-op op coeffs-a coeffs-b)
  (reduce-coeffs-iter
   (reverse (merge-sorted coeffs-a coeffs-b op))
   '()
  )
 )

 (define (bind-linear-call op)
  (define coeffs-call (curry linear-coeffs-op op))
  (lambda (a b) (coeffs-call a b))
 )

 ; We use general arithmetics on the cefficients.
 (define add-dense-coeffs (bind-linear-call add))
 (define sub-dense-coeffs (bind-linear-call sub))


 ; Mutiplies coeffs list by single coeff reversing the order.
 (define (mul-coeffs-by-coeff-iter coeffs coeff res)
  (if (null? coeffs) res
   (mul-coeffs-by-coeff-iter (cdr coeffs) coeff
    (cons (mul (car coeffs) coeff) res)
   )
  )
 )

 ; Dense terms contains a lot of zeros, and we speed up.
 (define (mul-coeffs-by-coeff coeffs coeff)
  (if (zero? coeff) (list coeff) ;<— not an empty list
   (mul-coeffs-by-coeff-iter coeffs coeff '())
  )
 )

 ; Takes reversed coeffs and shifts them by the order.
 (define (shift-coeffs-iter coeffs order res)
  (cond
   ((and (= 0 order) (null? coeffs)) res)

   ((null? coeffs) ; Done reversing, insert zeros?
    (shift-coeffs-iter '() (- order 1) (cons 0 res))
   )

   (else ; Just reverse the coeffs order:
    (shift-coeffs-iter (cdr coeffs) order (cons (car coeffs) res))
   )
  )
 )

 (define (mul-dense-coeffs-iter res order coeffs-a coeffs-b)
  (if (null? coeffs-b) res
   (mul-dense-coeffs-iter
    (add-dense-coeffs
     res
     (shift-coeffs-iter
      (mul-coeffs-by-coeff coeffs-a (car coeffs-b))
      order '()
     )
    )
    (+ order 1) coeffs-a (cdr coeffs-b)
   )
  )
 )

 (define mul-dense-coeffs (curry mul-dense-coeffs-iter '() 0))


 ; Resulting functions:
 (list add-dense-coeffs sub-dense-coeffs mul-dense-coeffs)
)
