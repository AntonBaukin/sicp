(include "sorted-set.scm")
(include "tree-interface.scm")


; This low-lever function allows to create arbitrary binary
; trees implemented in this package regardless of the order
; specified by the comparator.
;
; This may be allowed for test purposes where the order
; of the items has no meaning. We do not include genaral
; maker in abstract tree interface as only the order and
; tree balance features define what's on the left-right.
;
(define (make-tree-node item left right)
 (cons item (cons left right))
)

; Creates collection of ops for general binary tree.
; Use tree-op-* interface to access these operations.
; Comparator smaller? is a binary predicate.
(define (make-tree smaller?)
 (define Set (make-sorted-set smaller?))
 (define make-set (set-op-make Set))
 (define null '())

 (define (make-root item)
  (cons item (cons null null))
 )

 (define (get node)
  (car node)
 )

 (define (get-left node)
  (cadr node)
 )

 (define (get-right node)
  (cddr node)
 )

 (define (search-node node item)
  (cond
   ((null? node) null)

   ((smaller? item (get node))
    (search-node (get-left node) item)
   )

   ((smaller? (get node) item)
    (search-node (get-right node) item)
   )

   (else node) ;<— found it
  )
 )

 (define (search tree item)
  (let ((node (search-node tree item)))
   (if (null? node) null (get node))
  )
 )

 (define (tree->list t)
  (if (null? t) t
   (append
    (tree->list (get-left t))
    (cons (get t) (tree->list (get-right t)))
    )
  )
 )

 ; Goes to the first item dividing the list by 2 on each step
 ; combining the left and rights sub-trees with the center
 ; item to be the node (local root) up to the very center
 ; of the initial list making it the resulting root.
 ; Takes o(n) steps.
 (define (partial-tree elements n)
  (if (= 0 n) (cons null elements)
   (let* (
     (left-size (quotient (- n 1) 2))
     (left-result (partial-tree elements left-size))
     (left-tree (car left-result))
     (non-left-elements (cdr left-result))
     (right-size (- n (+ left-size 1)))   ; +1 as we take leading item
     (this-entry (car non-left-elements)) ; as this-entry of the node
     (right-result (partial-tree (cdr non-left-elements) right-size))
     (right-tree (car right-result))
     (remaining-elements (cdr right-result))
    )

    (cons
     (make-tree-node this-entry left-tree right-tree)
     remaining-elements
    )
   )
  )
 )

 (define (list->tree sequence)
  (let ((seq (make-set sequence)))
   (car (partial-tree seq (length seq)))
  )
 )

 (define (iter-right node cb)
  (let ((res (cb (get node))))
   ; Invoke callback for the current node:
   (if (eq? void res)
    ; Recurse into the right node:
    (if (null? (get-right node))
     void ;<— finished the iteration for this node
     (iter (get-right node) cb)
    )
    res ;<— return this result, break the iteration
   )
  )
 )

 (define (iter node cb)
  (if (null? (get-left node))
   (iter-right node cb)
   ; First, go left, then this and right:
   (let ((res (iter (get-left node) cb)))
    (if (eq? void res)
     (iter-right node cb)
     res ;<— the left had breaked
    )
   )
  )
 )

 (list
  smaller?     ; 0
  Set          ; 1
  get          ; 2
  get-left     ; 3
  get-right    ; 4
  make-root    ; 5
  search       ; 6
  tree->list   ; 7
  list->tree   ; 8
  iter         ; 9
 )
)
