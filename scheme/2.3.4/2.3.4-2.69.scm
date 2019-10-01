(define (log . args) (for-each display args) (newline))

(include "huffman-code.scm")

(define sample-char-pairs '(
 ("A" 8) ("B" 3) ("C" 1) ("D" 1)
 ("E" 1) ("F" 1) ("G" 1) ("H" 1)
))


(log "Sample pairs: " sample-char-pairs " —> tree: ")

;(define (null-log . args) void)

(define test-tree (build-huffman-tree log sample-char-pairs))
(log (huffman-tree->str test-tree))
