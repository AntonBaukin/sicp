
; Implements task §2.5.3-2.92. Returns two functions:
; multi-add — that takes any number of polynomials of
; any variables (also, nested into coefficients); and
; multi-mul — that takes two polynomials.
;
; Here we do not register them as general functions
; instead of add() and mul() as this has no special
; application in the exercise.
;
(define (install-polynomial-multi-ops split-package group-package)

 (define poly-split    (car split-package))
 (define merge-entries (cadr split-package))
 (define poly-group    (car group-package))
 (define collect-vars  (cadr group-package))

 (define (poly-auto-group entries)
  (poly-group (collect-vars entries) entries)
 )

 ; Takes a list of splitted entries:
 (define (sum-entries ents-list)
  (merge-entries (apply append ents-list))
 )

 ; Takes arbitrary number of polynomials:
 (define (multi-add . polys)
  (poly-auto-group (sum-entries (map poly-split polys)))
 )


 ; Takes two entries of ('* coeff (var . order)...),
 ; multiplies the coefficients, and sums the orders.
 ; Pairs (var . order) are ordered by var name asc.
 (define (mul-entry a b)
  (append
   (list
    '*
    (drop-impl (mul (cadr a) (cadr b)))
   )
   (reverse (mul-vos (cddr a) (cddr b) '()))
  )
 )

 ; Compares (var . order) by variable name ascending.
 (define (vo<? a b)
  (string<?
   (symbol->string (car a))
   (symbol->string (car b))
  )
 )

 ; Sums orders of two (var . order) having the same var.
 (define (sum-vo a b)
  (cons (car a) (+ (cdr a) (cdr b)))
 )

 (define (mul-vos vos-a vos-b res)
  (cond
   ((and (null? vos-a) (null? vos-b))
    res
   )

   ((null? vos-a)
    (mul-vos '() (cdr vos-b) (cons (car vos-b) res))
   )

   ((null? vos-b)
    (mul-vos (cdr vos-a) '() (cons (car vos-a) res))
   )

   ; a-variable is smaller —> take it:
   ((vo<? (car vos-a) (car vos-b))
    (mul-vos (cdr vos-a) vos-b (cons (car vos-a) res))
   )

   ; b-variable is smaller —> take it:
   ((vo<? (car vos-b) (car vos-b))
    (mul-vos vos-a (cdr vos-b) (cons (car vos-b) res))
   )

   (else ; Same variables:
    (mul-vos (cdr vos-a) (cdr vos-b)
     (cons (sum-vo (car vos-a) (car vos-b)) res)
    )
   )
  )
 )

 (define (mul-entries-by ents e res)
  (if (null? ents) res
   (mul-entries-by (cdr ents) e
    (cons (mul-entry (car ents) e) res)
   )
  )
 )

 (define (mul-entries ents rest res)
  (if (null? rest) res
   (mul-entries ents (cdr rest)
    (merge-entries (append res (mul-entries-by ents (car rest) '())))
   )
  )
 )

 (define (multi-mul poly-a poly-b)
  (poly-auto-group
   (mul-entries
    (poly-split poly-a)
    (poly-split poly-b)
    '()
   )
  )
 )
 

 ; Resulting addition and multiplication:
 (list multi-add multi-mul)
)
