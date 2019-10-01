(define (log . args) (for-each display args) (newline))

; power v ^ n, where n — positive integer
(define (pow-int v n)
 (cond
  ((= n 0) 1)
  ((even? n) (square (pow-int v (/ n 2))))
  (else (* v (pow-int v (- n 1))))
 )
)

; (slow) power of greatest divider of integer v by integer p
(define (div-pow-int v p)
 (define (next vi i)
  (if
   (and
    (> vi 0)
    (= 0 (remainder vi p))
   )
   (next (/ vi p) (+ i 1))
   i
  )
 )

 (next v 0)
)

(define (test-div-pow-int v p)
 (log "div pow " p " ( " v " ) = " (div-pow-int v p))
)

(test-div-pow-int (* 8 9) 2)
(test-div-pow-int (* 27 1024) 3)

(define (pair-int a b)
 (* (pow-int 2 a) (pow-int 3 b))
)

(define (pair-a p)
 (div-pow-int p 2)
)

(define (pair-b p)
 (div-pow-int p 3)
)

(define (test-pair-int a b)
 (log "pair (" a  " " b ") is " (pair-int a b)
  ", reverse a = " (pair-a (pair-int a b))
  ", b = " (pair-b (pair-int a b))
 )
)

(test-pair-int 5 14)