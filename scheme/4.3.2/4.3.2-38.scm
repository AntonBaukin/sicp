(define (log . args) (for-each display args) (newline))

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
 (define (combine result items)
  (if (null? items) (reverse result)
   (let ((i (amb-of items)))
    (combine (cons i result) (exclude '() items i))
   )
  )
 )

 (define (neighbour a b)
  (= 1 (abs (- a b)))
 )

 (define (baker tuple) (list-ref tuple 0))
 (define (cooper tuple) (list-ref tuple 1))
 (define (fletcher tuple) (list-ref tuple 2))
 (define (miller tuple) (list-ref tuple 3))
 (define (smith tuple) (list-ref tuple 4))

 (define (result tuple)
  (list
   (list 'baker (baker tuple))
   (list 'cooper (cooper tuple))
   (list 'fletcher (fletcher tuple))
   (list 'miller (miller tuple))
   (list 'smith (smith tuple))
  )
 )

 ; Altered implementation of §4.3.2: it combines
 ; instead of requiring unique numbers in a tuple.
 (define (multiple-dwelling task-4.38?)
  (define t (combine '() '(1 2 3 4 5)))

  (require (not (= (baker t) 5)))
  (require (not (= (cooper t) 1)))
  (require (not (or (= (fletcher t) 1) (= (fletcher t) 5))))
  (require (> (miller t) (cooper t)))
  (require (not (neighbour (fletcher t) (cooper t))))

  (if (not task-4.38?)
   (require (not (neighbour (smith t) (fletcher t))))
  )

  (result t)
 )

 (global multiple-dwelling)
)

(log "——— Original task from §4.3.2 ——— " "\n"
 (eval-amb-results (multiple-dwelling #f)) "\n"
)

(log "——— Task 4.38 ——— " "\n"
 (eval-amb-results (multiple-dwelling #t))
)
;
; Task 4.38 has five solutions:
; (baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5)
; (baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3)
; (baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3)
; (baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)
; (baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1)
;
; You may also note the following table of exlusions.
; They are collected like a sudoku.
;
;            1 2 3 4 5
;  baker       x   x x
;  cooper    x   x   x
;  fletcher  x   x   x
;  miller    x x   x
;  smith       x   x
;
;  Exclusion of (baker 1) in the original task may only
;  done via tracing this variant — it's not obvious.
;
