(define (log . args) (for-each display args) (newline))

(define (sqrt x)
  (define (avg a b) (* 0.5 (+ a b)))
  (define (close? y) (< (abs (- (* y y) x)) 0.001))
  (define (step y) (avg y (/ x y)))

  (define (sqrt-next y i)
    (cond ((close? y) (log i ": " y) y) (else
      (log i ": " y)
      (sqrt-next (step y) (+ i 1))
    ))
  )

  (sqrt-next 1.0 0)
)

(sqrt 121)
