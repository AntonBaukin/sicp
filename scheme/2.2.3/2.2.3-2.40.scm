(define (log . args) (for-each display args) (newline))

(define (enumerate-range a b)
 (define (iter i res)
  (if (< i a) res (iter (- i 1) (cons i res)))
 )

 (iter b (list))
)

; (log "range [1 10]: " (enumerate-range 1 10))

; operation is (op acc v)
(define (fold-left sequence initial op)
 (define (iter res tail)
  (if (null? tail) res
   (iter (op res (car tail)) (cdr tail))
  )
 )

 (iter initial sequence)
)

(define (flatmap sequence sequence-producer)
 (fold-left (map sequence-producer sequence) (list) append)
)

;(log "flat map of ± 1 .. 5 is "
; (flatmap (enumerate-range 1 5) (lambda (i) (list i (- i))))
;)

(define (unique-pairs n)
 (flatmap
  (enumerate-range 1 (- n 1))
  (lambda (j)
   (map (lambda (i) (cons j i))
    (enumerate-range (+ j 1) n)
   )
  )
 )
)

;(log "unique pairs to 5: " (unique-pairs 5))

(define (smallest-divisor n)
 (define (divisor i) (= 0 (remainder n i)))
 (define (step i) (if (= i 2) 3 (+ i 2)))

 (define (next i)
  (if (> (square i) n) n
   (if (divisor i) i (next (step i)))
  )
 )

 (next 2)
)

(define (prime? n)
 (= n (smallest-divisor n))
)

(define (filter sequence match?)
 (reverse
  (fold-left sequence (list)
   (lambda (acc v) (if (match? v) (cons v acc) acc))
  )
 )
)

;(log "primes in range [1 100]: "
; (filter (enumerate-range 1 100) prime?)
;)

(define (prime-sum-pairs n)
 (filter (unique-pairs n)
  (lambda (p)
   (prime? (+ (car p) (cdr p)))
  )
 )
)

(define (flatstr sequence separator string-producer)
 (fold-left (map string-producer sequence) ""
  (lambda (acc s)
   (if (= 0 (string-length acc)) s
    (string-append acc separator s)
   )
  )
 )
)

;(log "numbers of 1 .. 10 are {"
; (flatstr (enumerate-range 1 10) ", " number->string) "}"
;)

(log "prime sum of pairs in range [1 10] are: "
 (flatstr (prime-sum-pairs 10) ", "
  (lambda (p)
   (string-append
    (number->string (car p))
    " + "
    (number->string (cdr p))
    " = "
    (number->string (+ (car p) (cdr p)))
   )
  )
 )
)
