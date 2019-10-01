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

(define (unique-triples n)
 (flatmap
  (enumerate-range 1 (- n 2))
  (lambda (k)
   (flatmap
    (enumerate-range (+ k 1) (- n 1))
     (lambda (j)
      (map (lambda (i) (list k j i))
       (enumerate-range (+ j 1) n)
      )
    )
   )
  )
 )
)

(log "unique triples to 5: " (unique-triples 5))

(define (filter sequence match?)
 (reverse
  (fold-left sequence (list)
   (lambda (acc v) (if (match? v) (cons v acc) acc))
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

(define (eq-sum-triples n s)
 (define (triple-sum triple)
  (fold-left triple 0 +)
 )

 (filter (unique-triples n)
  (lambda (triple) (= s (triple-sum triple)))
 )
)

(log "equals to 15 sum of triples in range [1 10] are: "
 (flatstr (eq-sum-triples 10 15) ", "
  (lambda (t)
   (string-append
    (number->string (car t))
    " + "
    (number->string (cadr t))
    " + "
    (number->string (caddr t))
    " = "
    (number->string (+ (car t) (cadr t) (caddr t)))
   )
  )
 )
)
