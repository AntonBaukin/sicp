(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")
(include "../4.3.1/eval-amb.scm")

(eval-basic (debug on))

(eval-basic
 (define (neighbour a b)
  (= 1 (abs (- a b)))
 )

 (define (distinct? items)
  (cond
   ((null? items) #t)
   ((null? (cdr items)) #t)
   ((member (car items) (cdr items)) #f)
   (else (distinct? (cdr items)))
  )
 )



 ; Altered implementation of §4.3.2: it combines
 ; instead of requiring unique numbers in a tuple.
 (define (multiple-dwelling messed?)
  (define baker (amb 1 2 3 4 5))
  (define cooper (amb 1 2 3 4 5))
  (define fletcher (amb 1 2 3 4 5))
  (define miller (amb 1 2 3 4 5))
  (define smith (amb 1 2 3 4 5))

  ; Naswer: No. We may replace multiple «require» calls
  ; to single one with «and». If we reorder the nested
  ; expressions by the computation demands ascending,
  ; we may slightly reduce overall test time.
  ;
  ; Our solution from «4.3.2-38.scm» to use distinct
  ; combinations of tuples does not gain the speed.
  ;
  (require
   (and
    (not (= baker 5))
    (not (= cooper 1))
    (> miller cooper)
    (not (or (= fletcher 1) (= fletcher 5)))
    (not (neighbour smith fletcher))
    (not (neighbour fletcher cooper))
    (or
     messed?
     (distinct? (list baker cooper fletcher miller smith))
    )
   )
  )

  (list
   (list 'baker baker)
   (list 'cooper cooper)
   (list 'fletcher fletcher)
   (list 'miller miller)
   (list 'smith smith)
  )
 )

 (global multiple-dwelling)
)

(reset-ts)
(log "——— Solution with distinct occupations: ——" "\n"
 (eval-amb-results (multiple-dwelling #f)) "\n"
 "compute time: " (ts) "\n"
)

(log "——— Number of solution with not distinct: ——" "\n"
 (length (eval-amb-lim 150 (multiple-dwelling #t)))
)
