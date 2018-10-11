(define (stdout . args) (for-each display args))
(define (log . args) (for-each display args) (newline))

(define (product-recursive a b cmp next term)
 (if (> (cmp a b) 0) 1
  (* (term a) (product-recursive (next a) b cmp next term))
 )
)

(define (cmp-numbers a b) (if (< a b) -1 (if (= a b) 0 +1)))

(define (increment i) (+ i 1))

(define (identity x) x)

(define (factorial-r n) (product-recursive 2 n cmp-numbers increment identity))

(define (test-factorial-r n)
 (log "recursive " n "! = " (factorial-r n))
)

;(test-factorial-r 4 )
;(test-factorial-r 10)

(define (product-iterative a b cmp next term)
 (define (iter product x)
  (if (> (cmp x b) 0) product
   (iter (* product (term x)) (next x))
  )
 )

 (iter 1 a)
)

(define (factorial-i n) (product-iterative 2 n cmp-numbers increment identity))

(define (test-factorial-i n)
 (log "iterative " n "! = " (factorial-i n))
)

;(test-factorial-i 4 )
;(test-factorial-i 10)

(define (call-0-to-n n f)
 (define (iter i arg)
  (if (not (< i n)) #t
   (iter (+ i 1) (f i))
  )
 )

 (iter 0 #f)
)

;(call-0-to-n 10 (lambda (i) (stdout i " ")))
;(newline)

(define (product-pi n)
 (define (numi i) (if (= 0 i) 1 (* 2 (+ 1 (quotient i 2)))))
 (define (deni i) (+ 1 (* 2 (quotient (+ 1 i) 2))))

 (* 4.0 (/ (product-iterative 0 n cmp-numbers increment numi)
  (product-iterative 0 n cmp-numbers increment deni)))
)

(log "pi = 3.14159265359")
(log "pi ~ " (product-pi 10000))
