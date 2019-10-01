
(define (rat-make n d)
 (define (gr-com-div a b)
  (define (next x y);  <-- x >= y
   (if (= 0 y) x
    (gr-com-div y (remainder x y))
   )
  )

  (if (> a b) (next a b) (next b a))
 )

 (define (sign a) (if (< a 0) -1 +1))

 (let ((gcd (gr-com-div (abs n) (abs d))))
  (cons (* (sign n) (sign d) (/ (abs n) gcd)) (/ (abs d) gcd))
 )
)

(define (rat-num r) (car r))
(define (rat-den r) (cdr r))

(define (rat-str r)
 (string-append
  (number->string (rat-num r))
  "/"
  (number->string (rat-den r))
 )
)

(define (rat-add a b)
 (let
  (
   (an (rat-num a))
   (ad (rat-den a))
   (bn (rat-num b))
   (bd (rat-den b))
  )

  (rat-make (+ (* an bd) (* bn ad)) (* ad bd))
 )
)

(define (rat-neg r)
 (rat-make (- (rat-num r)) (rat-den r))
)

(define (rat-sub a b)
 (rat-add a (rat-neg b))
)

(define (rat-mul a b)
 (rat-make
  (* (rat-num a) (rat-num b))
  (* (rat-den a) (rat-den b))
 )
)

(define (rat-div a b)
 (rat-make
  (* (rat-num a) (rat-den b))
  (* (rat-den a) (rat-num b))
 )
)
