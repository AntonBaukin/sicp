(include "../2.2.2/tree-print.scm")

; Takes tree ops, i.e. a tree «class» object, and item
; (node value) to string converter. The left branch goes
; above the right. If the node is leaf empty brances are
; not printed; empty branches are blank spaces.
;
; See «tree-print-str» function.
(define (make-tree->str-printer treeops item->string)

 (define (marker) #t)
 (define (novalue) #t)

 (define get (tree-op-get treeops))
 (define leaf? (tree-op-leaf? treeops))
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))

 ; Wraps node value to be marked item.
 (define (make-item node)
  (cons marker (get node))
 )

 ; Is this a marked wrapped node value.
 (define (item? x)
  (and (pair? x) (eq? marker (car x)))
 )

 ; Prints to string an item: wrapped node value.
 (define (i2s item)
  (if (eq? novalue item) ""
   (string-append " " (item->string (cdr item)))
  )
 )

 ; Converts the given tree instance (root node)
 ; the the list structure supported by the print
 ; function «tree-print-str».
 (define (convert-tree node)
  (if (leaf? node) (list (make-item node))
   (list (make-item node) (append
    (if (null? (left node)) (list novalue)
     (convert-tree (left node))
    )
    (if (null? (right node)) (list novalue)
     (convert-tree (right node))
    )
   ))
  )
 )

 ; Resulting printer function.
 (lambda (tree)
  (if (null? tree) ""
   (tree-print-str (convert-tree tree) item? i2s)
  )
 )
)
