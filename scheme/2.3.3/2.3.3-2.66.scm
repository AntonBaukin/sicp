(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define records '(
 (10    "John"    "Dow")
 (131   "Alice"   "Cooper")
 (87    "Mike"    "Madson")
 (124   "Morgan"  "Fresh")
 (14    "Paul"    "Anderson")
 (15    "Luke"    "Skywalker")
))

(define DataTree (make-tree
 (lambda (a b) (< (car a) (car b)))
))

(define database
 ((tree-op<-list DataTree) records)
)

(define (record->str r)
 (string-append
  (number->string (car r))
  ", "
  (cadr r)
  " "
  (caddr r)
 )
)

(define datatree->str (make-tree-str-printer DataTree record->str))

(log "Database tree\n" (datatree->str database))

(define (lookup db id)
 ((tree-op-search DataTree) db (list id))
)

(define (test-lookup db id)
 (let ((rec (lookup db id)))
  (log "lookup by id = " id " –> "
   (if (null? rec) "NOT FOUND" (record->str rec))
  )
 )
)

(test-lookup database 15)
(test-lookup database 87)
(test-lookup database 13)
