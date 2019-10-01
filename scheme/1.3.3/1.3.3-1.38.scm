(define (stdout . args) (for-each display args))
(define (log . args) (for-each display args) (newline))

; (num i) and (den i) return number for i = 1..n
(define (calc-continued-fraction n num den)
 (define (recurse i)
  (if (> i n) 0 (/ (num i) (+ (den i) (recurse (+ i 1)))))
 )

 (recurse 1)
)

(define (call-1-to-n n f)
 (define (iter i arg)
  (if (> i n) #t
   (iter (+ i 1) (f i))
  )
 )

 (iter 1 #f)
)

(define (calc-e n)
 (define (num i) 1.0)
 (define (den i)
  (if (< i 3) i
   (if (> (remainder (- i 2) 3) 0) 1
    (+ 2 (* 2 (quotient (- i 2) 3)))
   )
  )
 )

 (+ 2.0 (calc-continued-fraction n num den))
)

;(call-1-to-n 10 (lambda (i) (stdout i " ")))
;(newline)
;(call-1-to-n 10 (lambda (i) (stdout (den i) " ")))
;(newline)

(log "e\t2.7182818284590452353602874713527")

(define (test-calc-e n)
 (log "e[" n "] \t" (calc-e n))
)

(test-calc-e 5)
(test-calc-e 8)
(test-calc-e 10)