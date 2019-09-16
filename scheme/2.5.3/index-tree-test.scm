(define (logs . args) (for-each display args))
(define (log . args) (for-each display args) (newline))

(include "../2.3.3/tree.scm")
(include "../2.3.3/tree-print.scm")
(include "index-tree.scm")

(define index-tree->str
 (make-tree->str-printer IndexTree
  (lambda (kv) (string-append
   "[" (number->string (car kv)) " → "
    (symbol->string (cdr kv)) "]"
  ))
 )
)

(define sample (make-index-tree))
(define get (index-tree-get sample))
(define set (index-tree-set sample))

(define (log-sample what)
 (log "\n" what ":\n" (index-tree->str ((caddr sample))))
)

(define (log-get-range from to)
 (if (> from to) (newline)
  (begin
   (logs from " := " (get from) " ")
   (log-get-range (+ from 1) to)
  )
 )
)

(define (index-tree-items->str itree)
 (define s "")

 ((index-tree-iter itree)
  (lambda (k v)
   (set! s
    (string-append s
     "(" (number->string k)
     " " (symbol->string v)
     ") "
    )
   )
   void
  )
 )

 s
)

(log-get-range 0 1)

(set 1 'b)
(log-sample "Single item")
(log-get-range 0 2)

(set 2 'c)
(set 1 'B)
(log-sample "Two items")
(log-get-range 0 3)


(set 5 'd)
(set 8 'e)
(set 9 'f)
(set 11 'j)
(set 5 'D)
(log-sample "Six items")
(log-get-range 0 12)
(log "Items in order: " (index-tree-items->str sample))
