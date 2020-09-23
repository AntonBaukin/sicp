(include "../2.3.3/quick-sort.scm")

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

(define (filter match? sequence)
 (reverse
  (fold-left sequence (list)
   (lambda (acc v) (if (match? v) (cons v acc) acc))
  )
 )
)

; Board is a list of numbers 1..N where each
; item in 0-based position I is a 1-based index
; of queen row position in I+1 column.
;
; Maximum column index is set for the test purposes
; to see the recusion steps on the full-board.
(define (queens board-size max-column)
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

 (define (queen-boards column)
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

 (queen-boards max-column)
)

; First, sorts the results. Then removes reversed answers.
(define (normalize results)
 ; Compares two equal size lists of numbers.
 (define (smaller? a b)
  (cond
   ((null? a) #f)
   ((> (car a) (car b)) #f)
   ((= (car a) (car b)) (smaller? (cdr a) (cdr b)))
   (else #t)
  )
 )

 (define sorted (quick-sort smaller? results))

 (filter
  (lambda (a)
   (define b (member (reverse a) sorted))
   (or (null? b) (smaller? a (car b)))
  )
  sorted
 )
)

(define (test-board board-size max-column)
 (newline)
 (log "Board " board-size "x" max-column
  " ————————————————————————————————————————\n"
  (normalize (queens board-size max-column))
 )
)

(for-each (lambda (i) (test-board i i)) (enumerate-range 3 8))
