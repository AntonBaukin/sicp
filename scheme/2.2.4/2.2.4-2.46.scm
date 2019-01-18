(define (log . args) (for-each display args) (newline))

(define (make-vect x y)
 (cons x y)
)

(define (xcor-vect v)
 (car v)
)

(define (ycor-vect v)
 (cdr v)
)

(define (vect->str v)
 (string-append
  "("
  (number->string (xcor-vect v))
  " "
  (number->string (ycor-vect v))
  ")"
 )
)

(define (add-vect a b)
 (make-vect
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))
 )
)

(define (neg-vect v)
 (make-vect
   (- (xcor-vect v))
   (- (ycor-vect v))
 )
)

(define (sub-vect a b)
 (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))
 )
)

(define (scale-vect s v)
 (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))
 )
)

(define Va (make-vect 1.0 2.0))
(define Vb (make-vect 3.0 -2.0))

(log "vector a = " (vect->str Va))
(log "vector b = " (vect->str Vb))
(log "a + b = " (vect->str (add-vect Va Vb)))
(log "-b = " (vect->str (neg-vect Vb)))
(log "a - b = " (vect->str (sub-vect Va Vb)))
(log "2a + 0.5b = " (vect->str (add-vect (scale-vect 2 Va) (scale-vect 0.5 Vb))))
