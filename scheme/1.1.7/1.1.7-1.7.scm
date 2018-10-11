(define (log . args) (for-each display args) (newline))

(define (sqrt x)
  (define (avg a b) (* 0.5 (+ a b)))
  (define (step y) (avg y (/ x y)))
  (define (close? y yp) (< (abs (- y yp)) (* y 0.0001)))

  (define (sqrt-next y yp i)
    (cond ((close? y yp) (log i ": " y) y) (else
      (log i ": " y " dY = " (abs (- y yp)))
      (sqrt-next (step y) y (+ i 1))
    ))
  )

  (sqrt-next 1.0 0 0)
)

(sqrt 100000000)
