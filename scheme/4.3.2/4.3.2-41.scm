(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")

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

; Excludes items from 1..5 and executes
; lambda being the last argument.
(define (each-exc . of)
 (define λ (car (reverse of)))
 (define exs (reverse (cdr (reverse of))))

 (for-each λ (exclude '(1 2 3 4 5) (flatten exs)))
)

(define (nb a) ;<— neigbour floors
 (list a (- a 1) (+ a 1))
)

; As we use these lists for exclude, we do not care
; about negative floor numbers.
(define (above a)
 (list a (- a 1) (- a 2) (- a 3) (- a 4))
)

(define (multiple-dwelling)
 (define result '())

 (define (take baker cooper fletcher miller smith)
  (set! result
   (cons
    (list
     (list 'baker baker)
     (list 'cooper cooper)
     (list 'fletcher fletcher)
     (list 'miller miller)
     (list 'smith smith)
    )
    result
   )
  )
 )

 ; As we may see, procedural variant of the solution
 ; looks almost the same as in «4.3.2-40.scm».
 ;
 ; We may conclude that using Amb evaluator only
 ; to replace loops with handy variables is not fancy
 ; enough to loose the performance in 100 times...
 ;
 (each-exc 5
  (lambda (baker)
   (each-exc 1 baker
    (lambda (cooper)
     (each-exc 1 5 baker (nb cooper)
      (lambda (fletcher)
       (each-exc baker fletcher (above cooper)
        (lambda (miller)
         (each-exc baker cooper miller (nb fletcher)
          (lambda (smith)
           (take baker cooper fletcher miller smith)
          )
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (reverse result)
)

(reset-ts)
(log "——— Solution using plain Lisp: ——" "\n"
 (multiple-dwelling) "\n"
 "compute time: " (ts)
)
;
; Comparing with amb evaluator in task «4.3.2-40.scm», this
; procedural computation takes almost nothing: 0.001 second!
; Amb one took: 0.183 second...
;
