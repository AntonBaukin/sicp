(include "../2.2.2/tree-print.scm")

; Takes tree ops, i.e. a tree «class» object, and item
; (node value) to string converter. The left branch goes
; above the right. If the node is leaf empty brances are
; not printed; empty branches are blank spaces.
;
; See «tree-print-str» function.
;
; Raw function converts to string nodes.
; Ordinary function below converts the items.
;
(define (make-tree->str-printer-raw tree-ops node->string)
 (define (marker) #t)
 (define (novalue) #t)

 (define leaf? (tree-op-leaf? tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 ; Wraps node to be marked item.
 (define (make-item node)
  (cons marker node)
 )

 ; Is this a marked wrapped node value.
 (define (item? x)
  (and (pair? x) (eq? marker (car x)))
 )

 ; Prints to string an item: wrapped node value.
 (define (i2s item)
  (if (eq? novalue item) ""
   (string-append " " (node->string (cdr item)))
  )
 )

 ; Converts the given tree instance (root node)
 ; the the list structure supported by the print
 ; function «tree-print-str».
 (define (convert-tree node)
  (if (leaf? node) (list (make-item node))
   (list (make-item node) (append
    (if (null? (get-left node)) (list novalue)
     (convert-tree (get-left node))
    )
    (if (null? (get-right node)) (list novalue)
     (convert-tree (get-right node))
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

(define (make-tree->str-printer tree-ops item->string)
 (define get (tree-op-get tree-ops))
 (make-tree->str-printer-raw
  tree-ops
  (lambda (node) (item->string (get node)))
 )
)
