
; Takes two lists and iteration callbacks.
; When where-callback returns #f, iteration
; moves to the next a-item; #t — moves to
; the next b-item; else — moves both.
;
; Where-callback takes 4 arguments:
;  — a  current tail of a-list;
;  — b  current tail of b-list;
;  — w  previous where-result, or void.
;  — t  previous take-result, or '().
;
; Take-callback has the same 4 arguments, but «w»
; means current where-result. The result of take
; is passed to next step, or if the final result,
; thus it's an accumulator.
;
; Take-callback is invoked before the move!
; Iteration stops when two lists are empty.
;
(define (iterate-two seqa seqb where take)
 (define (done? a b)
  (and (null? a) (null? b))
 )

 (define (next a b w t)
  (cond
   ((and (null? a) (null? b)) t)

   ((null? a)
    (next a (cdr b) #t (take a b #t t))
   )

   ((null? b)
    (next (cdr a) b #f (take a b #f t))
   )

   (else
    (let* (
      (w! (where a b w t))
      (t! (take a b w! t))
     )

     (cond
      ((eq? #f w!)
       (next (cdr a) b w! t!)
      )

      ((eq? #t w!)
       (next a (cdr b) w! t!)
      )

      (else (next (cdr a) (cdr b) w! t!))
     )
    )
   )
  )
 )

 (next seqa seqb void '())
)

; Merges two sorted lists using iterate-two.
; Merge takes (a b) to compare and returns: #f when
; «a» is smaller, #t when «b» is smaller, or merged
; value when they are equal, thus moving left, right,
; or both lists respectively.
;
(define (merge-sorted seqa seqb merge)
 (define (where a b w t)
  (merge (car a) (car b))
 )

 (define (take a b w t)
  (cond
   ((eq? #f w) (cons (car a) t))
   ((eq? #t w) (cons (car b) t))
   (else (cons w t))
  )
 )

 (reverse (iterate-two seqa seqb where take))
)


;(display (merge-sorted
; '(1 2 3 4 5 7 10 11)
; '(0 2 4 6 7 8 9 10 11 12)
;
; (lambda (a b) (if (= a b) a (> a b)))
;))

;(display (merge-sorted
; '((100 . 2) (10 . 2) (0 . 1))
; '((10 . -2) (0 . 1))
;
; (lambda (a b)
;  (cond
;   ((< (car a) (car b)) #t)
;   ((< (car b) (car a)) #f)
;   (else (cons (car a) (+ (cdr a) (cdr b))))
;  )
; )
;))
;
;(newline)
