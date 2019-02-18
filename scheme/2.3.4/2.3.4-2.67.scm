(define (log . args) (for-each display args) (newline))

(include "huffman-code.scm")

(define node  make-huffman-tree-node)
(define leaf  make-huffman-tree-leaf)
(define merge merge-huffman-nodes)

(define sample-tree
 (merge
  (leaf 4 "A")
  (merge
   (leaf 2 "B")
   (merge
    (leaf 1 "D")
    (leaf 1 "C")
   )
  )
 )
)

(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(log "\nSample Huffman tree\n" (huffman-tree->str sample-tree))
(log "Decoding " sample-bits)
(define sample-message (huffman-decode sample-tree sample-bits))
(log ":> " sample-message "\n")
