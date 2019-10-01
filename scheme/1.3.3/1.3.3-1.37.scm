(define (log . args) (for-each display args) (newline))

; (num i) and (den i) return number for i = 1..n
(define (calc-continued-fraction-r n num den)
 (define (recurse i)
  (if (> i n) 0
   (/ (num i) (+ (den i) (recurse (+ i 1))))
  )
 )

 (recurse 1)
)

(define (one i) 1.0)

(define (calc-φ-r n)
 (calc-continued-fraction-r n one one)
)

(define (test-calc-φ n what calc-φ)
 (define (next i)
  (cond ((> i n) void)
   (else
    (log what " φ [" i "] = " (/ 1.0 (calc-φ i)))
    (next (+ i 1))
   )
  )
 )

 (next 1)
)

(test-calc-φ 15 "re" calc-φ-r)

(define (calc-continued-fraction-i n num den)
 ; Ri = (Ai + Bi * Ri+1) / (Xi + Yi * Ri+1)
 (define (transform i a b)
  (if (> i n) a
   (transform (+ i 1) (+ (* a (den i)) (* b (num i))) a)
  )
 )

 (/ (transform 2 (num 1) 0) (transform 2 (den 1) 1))
)

(define (calc-φ-i n)
 (calc-continued-fraction-i n one one)
)

(test-calc-φ 15 "it" calc-φ-i)
