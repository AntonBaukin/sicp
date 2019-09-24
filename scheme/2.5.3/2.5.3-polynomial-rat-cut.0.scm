
; Set this flag to debug polynomial GCD — see 2.95 task.
(define trace-sparse-polynomial-cut #f)

; Creates cut() function for sparse polynomials that
; reduce the terms as it's in 2.94 task.
(define (make-sparse-polynomial-cut-0)
 (define make-poly (list-ref polynomial-package 0))

 (define (remainder-terms a b)
  (let* (
    (pa (make-poly '? a))
    (pb (make-poly '? b))
    ; Division result from 2.91 already has the remainder
    ; as the second part of the returned pair:
    (ir (div pa pb))
   )
   ; Here we unwrap sparse terms:
   (apply-generic-unwrap
    ; Here we unwrap phony polynomial:
    (cdr (apply-generic-unwrap (cdr ir)))
   )
  )
 )

 (define (div-terms a b)
  (let* (
    (pa (make-poly '? a))
    (pb (make-poly '? b))
    (ir (div pa pb))
   )
   ; Here we unwrap sparse terms:
   (apply-generic-unwrap
    ; Here we unwrap phony polynomial:
    (cdr (apply-generic-unwrap (car ir)))
   )
  )
 )

 (define (trace-gcd a b)
  (if (not trace-sparse-polynomial-cut) void
   (log "GCD " a " ÷ " b)
  )
 )

 (define (gcd-terms a b)
  (trace-gcd a b)
  (if (null? b) a
   (gcd-terms b (remainder-terms a b))
  )
 )

 ; Checks whether the terms are of polynomial with
 ; a term order being not zero.
 (define (terms-poly? terms)
  (and
   (not (null? terms))
   (> (caar terms) 0)
  )
 )

 (define (make-cut-terms gcd)
  ; Resulting cut operation:
  (lambda (terms-a terms-b)
   (let ((g (gcd terms-a terms-b)))
    (if (not (terms-poly? g))
     (cons terms-a terms-b) ;<— do not divide them
     (cons
      (div-terms terms-a g)
      (div-terms terms-b g)
     )
    )
   )
  )
 )


 (list ;<— collection of resulting functions
  (make-cut-terms gcd-terms) ; 0
  div-terms                  ; 1
  remainder-terms            ; 2
  gcd-terms                  ; 3
  make-cut-terms             ; 4
  terms-poly?                ; 5
 )
)
