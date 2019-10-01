(define (qbrt x)
  (define (avg a b) (/ (+ a b) 3.0))
  (define (abs a) (if (< a 0) (- a) a))
  (define (close? y) (< (abs (- (* y y y) x)) 0.001))
  (define (step y) (avg (* 2 y) (/ x (* y y))))
  (define (qbrt-next y) (if (close? y) y (qbrt-next (step y))))
  (qbrt-next 1.0)
)

(display (qbrt 27))
(newline)

(display (qbrt 8))
(newline)

(display (qbrt 3))
(newline)

