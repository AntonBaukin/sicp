(define (log . args) (for-each display args) (newline))

(include "../2.3.3/quick-sort.scm")
(include "../4.1.6/filter.scm")
(include "../4.3.1/eval-amb.scm")

(eval-basic (debug on))

(eval-basic
 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (define (exclude result items i)
  (cond
   ((null? items) (reverse result))
   ((eq? i (car items))
    (reverse (append (reverse (cdr items)) result))
   )
   (else
    (exclude (cons (car items) result) (cdr items) i)
   )
  )
 )

 ; Builds all combinations of the given items.
 (define (combine result items valid?)
  (if (null? items)
   (begin
    (require (valid? result))
    (reverse result)
   )
   (let ((i (amb-of items)))
    (require (valid? (cons i result)))
    (combine (cons i result) (exclude '() items i) valid?)
   )
  )
 )

 ; Tests board of size N assuming that board of size N-1 is safe.
 ; On each step of out combinations we place single qeen in a column,
 ; and test whether this qeen beats any of placed before.
 ;
 (define (safe-board? board)
  (define (test step tail)
   (if (null? tail) #t
    (and
     (not
      (or
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

 ;
 ; We combine numbers 1..8 into 8 positions.
 ; This automatically exludes vertical beating as only
 ; one qeen is placed in a column, and horizontal one,
 ; as unique number denotes unique board row.
 ;
 ; Check of diagonal beating must be solved while
 ; combining as this drastically reduces the overall
 ; number of large boards to check.
 ;
 (define (solve-queens)
  (combine '() '(1 2 3 4 5 6 7 8) safe-board?)
 )

 (global solve-queens)
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

 (filter sorted
  (lambda (a)
   (define b (member (reverse a) sorted))
   (or (null? b) (smaller? a (car b)))
  )
 )
)

(log "——— Solve the problem of chess queens ——— " "\n"
 (normalize (eval-amb-results (solve-queens))) "\n"
)
;
; The results (46, reversed omitted) are exactly the same
; as in mentioned task «2.2.3-2.42.scm».
;
