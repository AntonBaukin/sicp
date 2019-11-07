(define (log . args) (for-each display args) (newline))
(define (null-log . args) void)

(include "huffman-code.scm")
(include "../2.3.3/tree-util-max.scm")

(define (gen-abc-pairs n)
 (define (pair i)
  (list (number->string i) (expt 2 (- n 1 i)))
 )

 (define (next i res)
  (if (= i n) res
   (next (+ i 1) (cons (pair i) res))
  )
 )

 (reverse (next 0 '()))
)

(define abc-5 (gen-abc-pairs 5))
(define tree-5 (build-huffman-tree null-log abc-5))
(log "\nAbc 5 " abc-5 "\n" (huffman-tree->str tree-5))


(define abc-10 (gen-abc-pairs 10))
(define tree-10 (build-huffman-tree null-log abc-10))
(log "\nAbc 10 " abc-10 "\n" (huffman-tree->str tree-10))

(define (huffman-tree-depth tree)
 (define left (tree-op-left HuffmanTree))
 (define right (tree-op-left HuffmanTree))

 (define (depth t)
  (if (null? t) 0
   (+ 1 (max (depth (left t)) (depth (right t))))
  )
 )
 
 (depth tree)
)

(define (haffman-tree-max tree)
 (let* (
   (n ((make-tree-get-max HuffmanTree) tree))
   (c (car (get-huffman-chars n)))
   (d (huffman-encode-dict tree))
   (b (huffman-encode-flat d (list c)))
  )
  
  (list c "—>" b)
 )
)

(log "Abc 5 tree depth = " (huffman-tree-depth tree-5)
 " longest code = " (haffman-tree-max tree-5))

(log "Abc 10 tree depth = " (huffman-tree-depth tree-10)
 " longest code = " (haffman-tree-max tree-10))

(log "Abc N tree has depth N, shortest code is always (0) "
 "longest N-1 (1 .. 1)")
