(include "table-base.scm")
(include "../3.3.2/iterate.scm")

; Table is a list of pairs (key . value).
; To compare keys on equality keys-eq? is used.
(define (make-table keys-eq?)
 (define (make) '())

 (define (search tail key)
  (if (null? tail) void
   (if (keys-eq? key (caar tail))
    (car tail) ;<— return (key . value)
    (search (cdr tail) key)
   )
  )
 )

 (define (save table key value)
  (cons (cons key value) table)
 )

 (define (rewrite table kv value)
  (set-cdr! kv value)
  table
 )
 
 (define (iter table visitor)
  (iterate-list table
   (lambda (kv)
    (let ((res (visitor (car kv) (cdr kv))))
     (cond
      ((eq? #f res) #f) ;<— do break
      ((eq? void res) void)
      (else (set-cdr! kv res) void)
     )
    )
   )
  )
 )

 (make-table-base make search save rewrite length iter)
)
