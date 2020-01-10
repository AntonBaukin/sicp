
; In Gambit Scheme «void» is define as anonymous
; function, but we may define it as a «true» void
; based on short variant of if-form.
(define void
 (
  (lambda ()
   (define (get x) (if (> x 1) #t))
   (get 0)
  )
 )
)

(if (not (eq? void void))
 (error "Not a true void is defined!")
)
