(define (avg a b) (* 0.5 (+ a b)))

; standard (abs) exists...
(define (abs a) (if (< a 0) (- a) a))

(define (close? y x) (< (abs (- (* y y) x)) 0.001))

(define (step y x) (avg y (/ x y)))

(define (sqrt-next y x) (if (close? y x) y (sqrt-next (step y x) x)))

(define (sqrt x) (sqrt-next 1.0 x))

(display (sqrt 9))
(newline)

(display (sqrt 4))
(newline)

(display (sqrt 2))
(newline)
