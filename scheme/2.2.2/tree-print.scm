; Prints a tree in the same way the the shell program does.
; 
; Tree must be encoded in the following way: each tree node
; is a list of items; after an item a single subtree may be.
; So, the first element of a list is always an item, and no
; two or more consecutive lists may be.
;
; This produces a classical tree where a node is an item.
; SICP §2.2.2 allows any combination of items and lists making
; a node to be a transient knot, and each item — a leaf.
;
; Not a pair is always an item. A pair is checked with item?
; predicate from the argument. Pass #f when items are not
; structured objects. (Hint: empty list may be an item.)
;
(define (tree-print-str tree item? item->string)
 (define  N "\n")
 (define  L "─")
 (define  V "│")
 (define (X level) L)
 (define (U level) "└")
 (define (F level) (if (= 0 level) "┌" "├"))
 (define (M level) "├")

 (define (is-item? x)
  (if
   (not (pair? x)) #t
   (if (eq? #f item?) #f
    (item? x)
   )
  )
 )

 (define (has-item? list)
  (if (null? list) #f
   (or
    (is-item? list)
    (is-item? (car list))
    (has-item? (cdr list))
   )
  )
 )

 (define (indent level ups)
  (define (up-bar i)
   (if (has-item? (list-ref ups i)) V " ")
  )

  (define (iter str i)
   (if (= i level) str
    (iter (string-append str (up-bar i) " ") (+ i 1))
   )
  )

  (iter "" 0)
 )

 (define (append-ups ups tail)
  (append ups (list tail))
 )

 (define (item-level level ups item tail bar)
  (if (is-item? item)
   (string-append (indent level ups) (bar level) L (item->string item) N)
   (start-text-tree (+ level 1) (append-ups ups tail) item)
  )
 )

 (define (single-item-level level ups item tail)
  (item-level level ups item tail (if (= 0 level) X U))
 )

 (define (last-item-level   level ups item tail)
  (item-level level ups item tail U)
 )

 (define (first-item-level  level ups item tail)
  (item-level level ups item tail (if (has-item? tail) F U))
 )

 (define (middle-item-level level ups item tail)
  (item-level level ups item tail (if (has-item? tail) M U))
 )

 (define (text-level text level ups first? item tail)
  (string-append text
   (if (null? tail)
    (if first?
     (single-item-level level ups item tail)
     (last-item-level level ups item tail)
    )

    (if first?
     (first-item-level level ups item tail)
     (middle-item-level level ups item tail)
    )
   )
  )
 )

 (define (text-level-iter text level ups first? item tail)
  (let
   ((text-next (text-level text level ups first? item tail)))

   (if (null? tail) text-next
    (text-level-iter text-next level ups #f (car tail) (cdr tail))
   )
  )
 )

 (define (start-text-tree level ups item)
  (text-level-iter "" level ups #t
   (if (is-item? item) item (car item))
   (if (is-item? item) (list) (cdr item))
  )
 )

 (start-text-tree 0 (list) tree)
)


; Prints SICP §2.2.2 tree by converting it to the format
; defined in the above routine. There is no restriction
; on mess of the nested items and lists. Predicate item?
; has the same purpose: to tell a list from an item, it
; is invoked for pairs only.
;
; In the result tree a node is a knot displayed as circle.
; Each item is always a leaf, and each list being not an
; item is a subtree starting with knot.
;
(define (tree-knots-print-str tree item? item->string)

 (define (is-item? x)
  (if
   (not (pair? x)) #t
   (if (eq? #f item?) #f
    (item? x)
   )
  )
 )

 (define knot (lambda () #t))

 (define (add-item res item)
  (append res (list item))
 )

 (define (add-subtree res tree-list)
  (append res (list knot (convert (list) tree-list)))
 )

 (define (convert res tree-list)
  (if (null? tree-list) res
   (if (is-item? (car tree-list))
    (convert (add-item res (car tree-list)) (cdr tree-list))
    (convert (add-subtree res (car tree-list)) (cdr tree-list))
   )
  )
 )

 (if (is-item? tree)
  (tree-print-str tree item? item->string)
  (tree-print-str (convert (list) tree)
   (lambda (x) (or (eq? x knot) (is-item? x)))
   (lambda (item) (if (eq? item knot) "○" (item->string item)))
  )
 )
)

;; —————————————— Test Cases ——————————————
;;
;; —— select one of the routines to test ——
;(define tree-print-tested tree-print-str)
;(define tree-print-tested tree-knots-print-str)
;
;(define (log-numbers-tree tree)
; (display "tree ") (display tree) (newline)
; (display (tree-print-tested tree #f (lambda (item)
;  (string-append " " (number->string item)))))
; (newline)
;)
;
;(newline)
;(log-numbers-tree 1)
;(log-numbers-tree (list 2 3 4 5))
;(log-numbers-tree (list 2 3 4 (list 5 6)))
;(log-numbers-tree (list 2 3 (list 4 (list 5 6))))
;(log-numbers-tree (list 2 3 4 (list 5 6) 7))
;(log-numbers-tree (list 2 3 4 (list 5 6) 7 8))
;(log-numbers-tree (list 2 3 4 (list 5 (list 6 7 8))))
;(log-numbers-tree (list 2 3 4 (list 5 6 (list 7 8) 9)))
;(log-numbers-tree (list 2 3 4 (list 5 6 (list 7 8) 9) 10))
;
;
;; —— following tests work only with knot notation ——
;(define tree-print-tested tree-knots-print-str)
;
;(log-numbers-tree (list (list 1)))
;(log-numbers-tree (list (list 1) 2 (list 3) (list (list 4))))
;(log-numbers-tree (list (list 1) 2 (list 3) (list (list 4 5))))
;(log-numbers-tree (list (list 1) 2 (list 3) (list (list 4)) 5))
