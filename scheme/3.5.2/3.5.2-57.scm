(define (log . args) (for-each display args) (newline))

; Here we might use Gambit macros instead of lambda,
; as in «../3.5.1/stream.scm»
(define (cons-stream item then-lambda)
 (cons item then-lambda)
)

(define (stream-car s)
 (car s)
)

(define (stream-cdr s)
 ((cdr s)) ;<— call lambda instead of «force»
)

(define stream-null? null?)

(define (stream-ref stream i)
 (if (= i 0)
  (stream-car stream)
  (stream-ref (stream-cdr stream) (- i 1))
 )
)

(define (sub-stream->list n s)
 (cond
  ((stream-null? s) '())
  ((= 0 n) '())
  (else
   (cons
    (stream-car s)
    (sub-stream->list (- n 1) (stream-cdr s))
   )
  )
 )
)

(define sums-count 0)

; Simplified, direct implementation
; of two infinite streams addition.
(define (add-streams a b)
 (cons-stream
  (begin
   (set! sums-count (+ sums-count 1))
   (+ (stream-car a) (stream-car b))
  )
  (lambda ()
   (add-streams (stream-cdr a) (stream-cdr b))
  )
 )
)

; The same implementation as in «streams-test.scm»
; for regular streams with delayed expression.
(define fibs
 (cons-stream
  0
  (lambda ()
   (cons-stream
    1
    (lambda () ;<— use lambda instead of «delay»
     (add-streams fibs (stream-cdr fibs))
    )
   )
  )
 )
)

(define (log-sums n)
 (set! sums-count 0)
 (log " [" n "] = " (stream-ref fibs n) ",\tsums: " sums-count)
)

(log "Fibonacci stream with λ, first 20 items:")
(log (sub-stream->list 20 fibs))
(map log-sums '(3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

; As we see, number of sums multiplies around 1.7 on each step:
; [3] = 2,       sums: 3
; [4] = 3,       sums: 7
; [5] = 5,       sums: 14
; [6] = 8,       sums: 26
; [7] = 13,      sums: 46
; [8] = 21,      sums: 79
; [9] = 34,      sums: 133
; [10] = 55,     sums: 221
; [11] = 89,     sums: 364
; [12] = 144,    sums: 596
; [13] = 233,    sums: 972
; [14] = 377,    sums: 1581
; [15] = 610,    sums: 2567
; [16] = 987,    sums: 4163
; [17] = 1597,   sums: 6746
; [18] = 2584,   sums: 10926
; [19] = 4181,   sums: 17690
