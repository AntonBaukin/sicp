(include "../2.3.3/sorted-set.scm")
(include "../2.3.3/tree.scm")
(include "../2.3.3/tree-print.scm")
(include "tree-iter.scm")

; Set ot characters.
(define CharsSet (make-sorted-set string-ci<?))

; Huffman tree has no natural order, thus — comparator.
(define HuffmanTree (make-tree void))

(define (make-huffman-leaf weight char)
 (list 'leaf char weight)
)

(define (make-huffman-tree-leaf weight char)
 (define huffman-tree-single (tree-op-single HuffmanTree))
 (huffman-tree-single (make-huffman-leaf weight char))
)

(define (make-huffman-node sum-weights chars-set)
 (list 'node chars-set sum-weights)
)

(define (make-huffman-tree-node sum-weights chars-set left right)
 (define huffman-tree-node (tree-op-node HuffmanTree))
 (huffman-tree-node (make-huffman-node sum-weights chars-set) left right)
)

(define (huffman-leaf? node)
 (eq? 'leaf (car node))
)

(define (huffman-node-leaf? node)
 (huffman-leaf? ((tree-op-get HuffmanTree) node))
)

(define (get-huffman-chars node)
 (if (huffman-leaf? node)
  (list (cadr node)) ;<— set is a list
  (cadr node)
 )
)

(define (get-huffman-tree-chars node)
 (get-huffman-chars ((tree-op-get HuffmanTree) node))
)

(define (get-huffman-weight node)
 (caddr node)
)

(define (get-huffman-tree-weight node)
 (get-huffman-weight ((tree-op-get HuffmanTree) node))
)

(define (huffman-accumulate sequence initial op)
 (define (iter tail res)
  (if (null? tail) res
   (iter (cdr tail) (op res (car tail)))
  )
 )

 (iter sequence initial)
)

(define (merge-huffman-nodes left right)
 (define chars-set-union (set-op-union CharsSet))

 (define (union-chars set node)
  (chars-set-union set (get-huffman-tree-chars node))
 )

 (define (sum-weights sum node)
  (+ sum (get-huffman-tree-weight node))
 )

 (make-huffman-tree-node
  (huffman-accumulate (list left right) 0 sum-weights)
  (huffman-accumulate (list left right) '() union-chars)
  left right
 )
)

(define (huffman-node->str node)
 (define (join-strs chars-set)
  (huffman-accumulate chars-set ""
   (lambda (res s) (if (= 0 (string-length res)) s
    (string-append res " " s)))
  )
 )

 (string-append
  (number->string (get-huffman-weight node))
  " { " (join-strs (get-huffman-chars node)) " }"
 )
)

(define (huffman-tree-node->str node)
 (huffman-node->str ((tree-op-get HuffmanTree) node))
)

(define huffman-tree->str (make-tree->str-printer
 HuffmanTree huffman-node->str)
)

(define (huffman-decode tree bits)
 (define result '())
 (define leaf? huffman-node-leaf?)
 (define tree-iter (make-tree-iter HuffmanTree))
 (define (char leaf) (car (get-huffman-tree-chars leaf)))

 (define (push leaf res)
  ;(log "push " (char leaf))
  (set! result (cons (char leaf) result))
  res
 )

 (define (navigate bit)
  ;(log "go by bit " bit)
  (set! bits (cdr bits))
  (cond
   ((= 0 bit) 'left)
   ((= 1 bit) 'right)
   (else (error (error "Wrong huffman bit: " bit)))
  )
 )

 ; By taking leaf node we done decoding current character
 ; and start from the root taking bit on the next take.
 (define (take node)
  ;(log "take " (huffman-tree-node->str node))
  (if (leaf? node) (push node 'root) (navigate (car bits)))
 )

 (define (done node)
  ;(log "done at " (huffman-tree-node->str node))
  (if (leaf? node) (push node #t)
   (error "Incomlete Huffman bits sequence!")
  )
 )

 ;~> this iterator first walks then checks
 (tree-iter tree (lambda (node from stack)
  (if (null? bits) (done node) (take node))
 ))

 (reverse result)
)


; Tree of pairs (character . bits sequence) used
; to rapidly encode messgae without tree walking.
(define HuffmanEncodeDict (make-tree
 (lambda (a b) (string-ci<? (car a) (car b)))
))

; Builds instance of EncodeDict for the given tree.
(define (huffman-encode-dict huffman-tree)
 (define get-leafs (make-tree-get-leafs HuffmanTree))
 (define make-dict (tree-op<-list HuffmanEncodeDict))
 (define left (tree-op-get-left HuffmanTree))
 (define (left? node parent) (eq? node (left parent)))

 (define (collect-bits way-up)
  (define (add-bit bits node parent)
   (cons (if (left? node parent) 0 1) bits)
  )

  (define (next tail bits)
   (if (= 1 (length tail)) bits
    (next (cdr tail) (add-bit bits (car tail) (cadr tail)))
   )
  )

  (next way-up '())
 )

 ; The way up starts from a leaf and walks to the root.
 (define (make-dict-entry way-up)
  (cons (car (get-huffman-tree-chars (car way-up)))
   (collect-bits way-up)
  )
 )

 (make-dict (map make-dict-entry (get-leafs huffman-tree)))
)


; Returns a list of bits lists each being a code of character.
; You need to make the result flat to get the code. Not known
; characters are placed instead of a bits list.
(define (huffman-encode dict msg)
 (define search (tree-op-search HuffmanEncodeDict))

 (define (add-bits bits-list bits-search char)
  (cons (if (null? bits-search) char (cdr bits-search)) bits-list)
 )

 (define (encode-next msg-tail bits-list)
  (if (null? msg-tail) bits-list
   (let ((bits (search dict msg-tail)))
    (encode-next (cdr msg-tail)
     (add-bits bits-list bits (car msg-tail))
    )
   )
  )
 )

 (reverse (encode-next msg '()))
)

(define (huffman-encode-flat dict msg)
 (huffman-accumulate
  (huffman-encode dict msg) '()
  (lambda (res bits)
   (append res (if (pair? bits) bits (list bits)))
  )
 )
)

(define (chars-set-smaller? set-a set-b)
 (define (next tail-a tail-b)
  (cond
   ((null? tail-a) (not (null? tail-b)))
   ((null? tail-b) #f)
   ((string-ci<? (car tail-a) (car tail-b)) #t) ;<— char smaller
   ((string-ci<? (car tail-b) (car tail-a)) #f) ;<— char greater
   (else (next (cdr tail-a) (cdr tail-b)))
  )
 )

 (next set-a set-b)
)

(define (huffman-tree-node-smaller? a b)
 (let (
   (wa (get-huffman-tree-weight a))
   (wb (get-huffman-tree-weight b))
  )
  (cond
   ((< wa wb) #t) ;<— compare by the weights
   ((> wa wb) #f)
   (else (chars-set-smaller?
    (get-huffman-tree-chars a)
    (get-huffman-tree-chars b)
   ))
  )
 )
)

; Takes list of pairs (with arbitrary order) and creates
; «instance» of HuffmanTree «class».
(define (build-huffman-tree log huffman-pairs)

 (define (nodes->str nodes)
  (huffman-accumulate nodes "" (lambda (res n)
   (string-append res " " (huffman-tree-node->str n))
  ))
 )

 (define (merge-two nodes)
  ; { only one pair is left } resulting node
  (if (null? (cdr nodes)) (car nodes)
   (let ((m (merge-huffman-nodes (car nodes) (cadr nodes))))
    (log "merged" (nodes->str (list (car nodes) (cadr nodes))))
    (sort-and-merge (cons m (cddr nodes)))
   )
  )
 )

 (define (sort-and-merge nodes)
  (let ((s (quick-sort huffman-tree-node-smaller? nodes)))
   (log "sorted" (nodes->str s))
   (merge-two s)
  )
 )

 (define (init-node p)
  (make-huffman-tree-leaf (cadr p) (car p))
 )

 (sort-and-merge (map init-node huffman-pairs))
)
