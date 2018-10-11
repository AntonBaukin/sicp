(define (sqrt x)
  (define (avg a b) (* 0.5 (+ a b)))
  (define (close? y) (< (abs (- (* y y) x)) 0.001))
  (define (step y) (avg y (/ x y)))
  (define (sqrt-next y) (if (close? y) y (sqrt-next (step y))))
  (sqrt-next 1.0)
)

(display (sqrt 9))
(newline)

(display (sqrt 4))
(newline)

(display (sqrt 2))
(newline)
