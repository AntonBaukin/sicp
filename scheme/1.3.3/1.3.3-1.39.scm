(define (stdout . args) (for-each display args))
(define (log . args) (for-each display args) (newline))

; (num i) and (den i) return number for i = 1..n
(define (calc-continued-fraction n num den)
 (define (recurse i)
  (if (> i n) 0 (/ (num i) (+ (den i) (recurse (+ i 1)))))
 )

 (recurse 1)
)

(define (calc-tangent n x)
 (let ((x2 (- 0.0 (square x))))
  (define (num i) (if (= 1 i) x x2))
  (define (den i) (- (* i 2) 1))

  (calc-continued-fraction n num den)
 )
)

(define (test-calc-tangent n)
 (define pi3 (/ 3.141592653589793 3))
 (log "tan (π/3) [" n "] \t" (calc-tangent n pi3))
)

(log "tan (π/3) \t" (sqrt 3))

(test-calc-tangent 3)
(test-calc-tangent 5)
(test-calc-tangent 9)
