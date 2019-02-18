(include "2.3.4-2.67.scm")

(log "Sample tree leafs: ")
(for-each (lambda (l) (log (huffman-tree-node->str (car l))))
 ((make-tree-get-leafs HuffmanTree) sample-tree)
)

(define sample-encode-dict (huffman-encode-dict sample-tree))
(log "\nEncode dictionary of the sample tree:\n" sample-encode-dict "\n")

(log "Encoded back sample message " sample-message)
(log (huffman-encode sample-encode-dict sample-message))
(log (huffman-encode-flat sample-encode-dict sample-message) "\n")
