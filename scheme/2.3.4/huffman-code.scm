(include "../2.3.3/sorted-set.scm")
(include "../2.3.3/tree.scm")
(include "../2.3.3/tree-print.scm")
(include "tree-iter.scm")

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
