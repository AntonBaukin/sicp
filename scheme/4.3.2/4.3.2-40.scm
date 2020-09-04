(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")
(include "../4.3.1/eval-amb.scm")

(eval-basic (debug on))

(eval-basic
 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (define (exclude items exs)
  (define (next result items)
   (cond
    ((null? items) (reverse result))
    ((member (car items) exs)
     (next result (cdr items))
    )
    (else
     (next (cons (car items) result) (cdr items))
    )
   )
  )

  (next '() items)
 )

 (define (flatten items)
  (define (next result items)
   (cond
    ((null? items) (reverse result))
    ((list? (car items))
     (next
      (append (reverse (car items)) result)
      (cdr items)
     )
    )
    (else
     (next (cons (car items) result) (cdr items))
    )
   )
  )

  (next '() items)
 )

 (define (amb-exc . exs)
  (amb-of (exclude '(1 2 3 4 5) (flatten exs)))
 )

 (define (nb a) ;<— neigbour floors
  (list a (- a 1) (+ a 1))
 )

 ; As we use these lists for exclude, we do not care
 ; about negative floor numbers.
 (define (above a)
  (list a (- a 1) (- a 2) (- a 3) (- a 4))
 )

 ; Here we use nested «lets» as task reqires limiting available
 ; options for «amb». We do not need any «require» checks!
 ;
 (define (multiple-dwelling)
  (let ((baker (amb-exc 5)))
   (let ((cooper (amb-exc 1 baker)))
    (let ((fletcher (amb-exc 1 5 baker (nb cooper))))
     (let ((miller (amb-exc baker fletcher (above cooper))))
      (let ((smith (amb-exc baker cooper miller (nb fletcher))))
       (list
        (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)
       )
      )
     )
    )
   )
  )
 )

 (global multiple-dwelling)
)

(reset-ts)
(log "——— Solution with selective permutations: ——" "\n"
 (eval-amb-results (multiple-dwelling)) "\n"
 "compute time: " (ts)
)
;
; Task «4.3.2-39.scm» measured 1.720 seconds,
; and this variant took: 0.183 second.
; So, it's 10 times faster!
;

