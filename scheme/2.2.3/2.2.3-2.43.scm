(define (log . args) (for-each display args) (newline))

(define (enumerate-range a b)
 (define (iter i res)
  (if (< i a) res (iter (- i 1) (cons i res)))
 )

 (iter b (list))
)

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

(define (flatstr sequence separator string-producer)
 (fold-left (map string-producer sequence) ""
  (lambda (acc s)
   (if (= 0 (string-length acc)) s
    (string-append acc separator s)
   )
  )
 )
)

(define (filter match? sequence)
 (reverse
  (fold-left sequence (list)
   (lambda (acc v) (if (match? v) (cons v acc) acc))
  )
 )
)

(define empty-board (list))

(define (adjoin-row row smaller-board)
 (cons row smaller-board)
)

; New column is added as 0-one, and we
; do not require it's index.
(define (safe? board)
 (define (test step tail)
  (if (null? tail) #t
   (and
    (not
     (or
      (= (car board) (car tail))
      (= (car board) (- (car tail) step))
      (= (car board) (+ (car tail) step))
     )
    )
    (test (+ step 1) (cdr tail))
   )
  )
 )

 (test 1 (cdr board))
)

(define (accumulator-make)
 (define acc (list))

 (define (inc v)
  (set! acc (cons v acc))
 )

 (define (count v)
  (define (iter res tail)
   (if (null? tail) res
    (iter (+ res (if (= v (car tail)) 1 0)) (cdr tail))
   )
  )
  
  (iter 0 acc)
 )

 (list inc count)
)

(define (accumulate acc v)
 ((car acc) v)
)

(define (accumulated-count acc v)
 ((cadr acc) v)
)

; Board is a list of numbers 1..N where each
; item in 0-based position I is a 1-based index
; of queen row position in I+1 column.
(define (queens board-size)
 (define acc (accumulator-make))

 (define (queen-boards column)
  (accumulate acc column)
  (if (= column 0) (list empty-board)
   (filter
    (lambda (board) (safe? board))
    (flatmap
     (queen-boards (- column 1))
     (lambda (smaller-board)
      (map (lambda (row) (adjoin-row row smaller-board))
       (enumerate-range 1 board-size)
      )
     )
    )
   )
  )
 )

 (define result (queen-boards board-size))
 (cons result acc)
)

(define (queens-slow board-size)
 (define acc (accumulator-make))

 (define (queen-boards column)
  (accumulate acc column)
  (if (= column 0) (list empty-board)
   (filter
    (lambda (board) (safe? board))

    ; By swapping lambdas we calculate fat recursive
    ; function Q (queen-boards) for each ror each time.
    ;
    ; Normal version makes Q(7) ... Q(1)
    ; Slow version makes 8*Q(7) 64*Q(6) 256*Q(5) ...

    (flatmap
     (enumerate-range 1 board-size)
     (lambda (row)
      (map (lambda (smaller-board) (adjoin-row row smaller-board))
       (queen-boards (- column 1))
      )
     )
    )
   )
  )
 )

 (define result (queen-boards board-size))
 (cons result acc)
)

(define (test-time queens-proc queens-proc-name board-size)
 (let (
   (ts (real-time))
   (boards (queens-proc board-size))
  )

  (log "Board " board-size "x" board-size " " queens-proc-name
   " results number: " (length (car boards)) " time: " (- (real-time) ts)
  )

  (log "calls: " (flatstr (enumerate-range 0 (- board-size 1)) ", "
   (lambda (i)
    (string-append "Q(" (number->string i) ") = "
      (number->string (accumulated-count (cdr boards) i))
    )
   )
  ))
 )
)

(test-time queens "normal version" 8)
;$> Board 8x8 normal version results number: 92 time: .1488339900970459
;$> calls: Q(0) = 1, Q(1) = 1, Q(2) = 1, Q(3) = 1, Q(4) = 1,
;    Q(5) = 1, Q(6) = 1, Q(7) = 1

(test-time queens-slow "slow version" 8)
;$> Board 8x8 slow version results number: 92 time: 233.13761019706726
;$> calls: Q(0) = 16777216, Q(1) = 2097152, Q(2) = 262144, Q(3) = 32768,
;    Q(4) = 4096, Q(5) = 512, Q(6) = 64, Q(7) = 8
;
;   It's 1566 times slower!
