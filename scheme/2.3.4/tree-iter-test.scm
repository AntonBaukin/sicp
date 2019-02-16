(define (log . args) (for-each display args) (newline))

(include "../2.3.3/tree.scm")
(include "../2.3.3/tree-print.scm")
(include "tree-iter.scm")

(define NumTree (make-tree <))
(define num-tree->str (make-tree->str-printer NumTree number->string))
(define num-list->tree (tree-op<-list NumTree))
(define num-tree-order-iter (make-tree-order-iter NumTree))
(define num-tree-tree-get-min (make-tree-get-min NumTree))
(define num-tree-tree-get-max (make-tree-get-max NumTree))

(define (num-tree-collector tree)
 (define result '())

 (num-tree-order-iter tree (lambda (v)
  (set! result (cons v result))
  '() ;<— always continue
 ))

 (reverse result)
)

(define (enumerate n)
 (define (iter i res)
  (if (< i 1) res (iter (- i 1) (cons i res)))
 )

 (iter n (list))
)

(define (test-tree-collect n)
 (let ((tree (num-list->tree (enumerate n))))
  (log "\nTree " (num-tree-collector tree) "\n" (num-tree->str tree) "\n")
 )
)

(define (test-tree-min tree)
 (log "\nTree min = " (num-tree-tree-get-min tree) "\n" (num-tree->str tree) "\n")
)

(define (test-tree-max tree)
 (log "\nTree max = " (num-tree-tree-get-max tree) "\n" (num-tree->str tree) "\n")
)

;(for-each test-tree-collect (enumerate 10))
(test-tree-min (num-list->tree (enumerate 10)))
(test-tree-max (num-list->tree (enumerate 10)))
