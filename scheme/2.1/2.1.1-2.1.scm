(define (log . args) (for-each display args) (newline))

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

;(log "1/3 = " (rat-str (rat-make 1 3)))
;(log "3/12 = " (rat-str (rat-make 3 12)))
;(log "-1/5 = " (rat-str (rat-make -1 5)))
;(log "2/-3 = " (rat-str (rat-make 2 -3)))
;(log "-18/-36 = " (rat-str (rat-make -18 -36)))

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

(log "1/3 + 4/5 = " (rat-str (rat-add (rat-make 1 3) (rat-make 4 5))))

(define (rat-neg r)
 (rat-make (- (rat-num r)) (rat-den r))
)

;(log "-1/3 = " (rat-str (rat-neg (rat-make 1 3))))

(define (rat-sub a b)
 (rat-add a (rat-neg b))
)

(log "4/5 - 1/3 = " (rat-str (rat-sub (rat-make 4 5) (rat-make 1 3))))

(define (rat-mul a b)
 (rat-make
  (* (rat-num a) (rat-num b))
  (* (rat-den a) (rat-den b))
 )
)

(log "7/10 * 2/3 = " (rat-str (rat-mul (rat-make 7 10) (rat-make 2 3))))

(define (rat-div a b)
 (rat-make
  (* (rat-num a) (rat-den b))
  (* (rat-den a) (rat-num b))
 )
)

(log "7/10 ÷ 3/2 = " (rat-str (rat-div (rat-make 7 10) (rat-make 3 2))))
