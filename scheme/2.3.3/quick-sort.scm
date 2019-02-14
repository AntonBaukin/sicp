;(define (log . args) (for-each display args) (newline))

(define (quick-sort smaller? sequence)

 ; Returns sub-lists of the left and the right halfs
 ; of the list. The first item of the right is center.
 ; For a list with even length the right list has +2
 ; items to the left half — the first item is center,
 ; and +1 gives priority to the right. The order of
 ; the left half is revesed as it's before the sort.
 ; Takes o(n), one pass, true iterative.
 (define (split seq)
  (let ((r (split-iter 0 '() seq)))
   (cons (car r) (caddr r))
  )
 )

 ; Returns (left left-length right right-length).
 (define (split-iter i left tail)
  (if (null? tail) (list left i '() 0)
   (let* (
     (p  (split-iter (+ i 1) (cons (car tail) left) (cdr tail)))
     (l  (car p))
     (ln (cadr p))
     (r  (caddr p))
     (rn (cadddr p))
    )

    (if (< (- ln rn) 0) p
     (list (cdr l) (- ln 1) (cons (car l) r) (+ rn 1))
    )
   )
  )
 )

 ; Takes two lists, the first of right is the center.
 ; Moves of all items from the right that smaller than
 ; the center to the left, and vice versa.
 (define (separate left_right)
  (if (null? (cdr left_right)) left_right
   (let* (
     (l (car left_right))
     (r (cdr left_right))
     (s (separate-iter (car r) l (cdr r) '() '()))
    )
    ;~> center goes to the left if the right is not empty
    (if (null? (cdr s)) (cons (car s) (list (car r)))
     (cons (cons (car r) (car s)) (cdr s))
    )
   )
  )
 )

 (define (separate-iter center ltail rtail left right)
  (cond

   ((not (or (null? ltail) (null? rtail)))
    (let* (
      (ls? (not (smaller? center (car ltail))))
      (rs? (not (smaller? center (car rtail))))
      (r_l (if rs? (cons (car rtail) left) left))
      (l_r (if ls? right (cons (car ltail) right)))
     )

     (separate-iter center (cdr ltail) (cdr rtail)
      (if ls? (cons (car ltail) r_l) r_l)
      (if rs? l_r (cons (car rtail) l_r))
     )
    )
   )

   ((not (null? rtail))
    (let ((rs? (not (smaller? center (car rtail)))))
     (separate-iter center ltail (cdr rtail)
      (if rs? (cons (car rtail) left) left)
      (if rs? right (cons (car rtail) right))
     )
    )
   )

   ((not (null? ltail))
    (let ((ls? (not (smaller? center (car ltail)))))
     (separate-iter center (cdr ltail) rtail
      (if ls? (cons (car ltail) left) left)
      (if ls? right (cons (car ltail) right))
     )
    )
   )

   (else (cons left right)) ;<— the result
  )
 )

 (define (two-sort seq)
  (cond
   ((null? seq) seq)
   ((null? (cdr seq)) seq)

   ((null? (cddr seq))
    (if (smaller? (car seq) (cadr seq)) seq
     (list (cadr seq) (car seq))
    )
   )

   (else (insort seq))
  )
 )

 (define (insort seq)
  (let* (
    (lr (separate (split seq)))
    (l  (car lr))
    (r  (cdr lr))
   )

   ;(log "insort " l " | " r)
   (if (or (null? l) (null? r)) seq
    (append (two-sort l) (two-sort r))
   )
  )
 )

 ;(log "split " sequence " := " (split sequence))
 ;(log "separate " sequence " := " (separate (split sequence)))
 ;(log "sort " sequence " := " (two-sort sequence))

 (two-sort sequence)
)

;(quick-sort < '(1 2 3 4 5))
;(quick-sort < '(1 2 3))
;(quick-sort < '(1 2 3 4 5 6))
;(quick-sort < '(1 2))
;(quick-sort < '(1))
;(quick-sort < '())

;(quick-sort < '(5 4 3 2 1))
;(quick-sort < '(3 6 5 4 1 5 2 1 2 3))
;(quick-sort < '(3 6 5 4 5 1 2 1 2 3))
