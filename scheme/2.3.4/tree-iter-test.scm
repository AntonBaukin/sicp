(define (log . args) (for-each display args) (newline))

(include "../2.3.3/tree.scm")
(include "../2.3.3/tree-print.scm")
(include "tree-iter.scm")

(define NumTree (make-tree <))
(define num-tree->str (make-tree->str-printer NumTree number->string))
(define num-list->tree (tree-op<-list NumTree))
(define num-tree->list (tree-op->list NumTree))
(define num-tree-order-iter (make-tree-order-iter NumTree))
(define num-tree-get-min (make-tree-get-min NumTree))
(define num-tree-get-max (make-tree-get-max NumTree))
(define num-tree-get-max-smaller (make-tree-get-max-smaller NumTree))
(define num-tree-get-min-greater (make-tree-get-min-greater NumTree))

(define (num-tree-collector tree)
 (define result '())

 (num-tree-order-iter tree (lambda (v)
  (set! result (cons v result))
  void ;<— always continue
 ))

 (reverse result)
)

(define (enumerate n)
 (define (iter i res)
  (if (< i 1) res (iter (- i 1) (cons i res)))
 )

 (iter n (list))
)

(define (filter sequence match?)
 (define (next tail res)
  (if (null? tail) res
   (next (cdr tail)
    (if (match? (car tail)) (cons (car tail) res) res)
   )
  )
 )

 (reverse (next sequence '()))
)

(define (drop-equal-nums sequence mod)
 (if (<= mod 1) sequence
  (filter sequence (lambda (x) (not (= 0 (remainder x mod)))))
 )
)

(define (test-tree-collect n)
 (let ((tree (num-list->tree (enumerate n))))
  (log "\nTree " (num-tree-collector tree) "\n" (num-tree->str tree) "\n")
 )
)

(define (test-tree-min tree)
 (log "\nTree min = " (num-tree-get-min tree) "\n" (num-tree->str tree) "\n")
)

(define (test-tree-max tree)
 (log "\nTree max = " (num-tree-get-max tree) "\n" (num-tree->str tree) "\n")
)

(define (tree-get-max-smaller n)
 (define (get-smaller tree i)
  (let ((sm (num-tree-get-max-smaller tree i)))
   (if (null? sm) "NO" (number->string sm))
  )
 )

 (define (num-test tree i res)
  (if (> i (+ n 1)) res
   (num-test tree (+ i 1) (string-append res " "
    (number->string i) "→" (get-smaller tree i)
   ))
  )
 )
 ;(drop-equal-nums sequence mod)
 (let* (
   (items (drop-equal-nums (enumerate n) (quotient n 3)))
   (tree (num-list->tree items))
  )
  (log "\nMax-smaller of tree " (num-tree->list tree)
   (num-test tree 1 "") "\n"
   (num-tree->str tree) "\n"
  )
 )
)

(define (tree-get-min-greater n)
 (define (get-greater tree i)
  (let ((gr (num-tree-get-min-greater tree i)))
   (if (null? gr) "NO" (number->string gr))
  )
 )

 (define (num-test tree i res)
  (if (> i n) res
   (num-test tree (+ i 1) (string-append res " "
    (number->string i) "→" (get-greater tree i)
   ))
  )
 )
 ;(drop-equal-nums sequence mod)
 (let* (
   (items (drop-equal-nums (enumerate n) (quotient n 3)))
   (tree (num-list->tree items))
  )
  (log "\nMin-greater of tree " (num-tree->list tree)
   (num-test tree 0 "") "\n"
   (num-tree->str tree) "\n"
  )
 )
)


(for-each test-tree-collect (enumerate 10))
(test-tree-min (num-list->tree (enumerate 10)))
(test-tree-max (num-list->tree (enumerate 10)))
(for-each tree-get-max-smaller (enumerate 10))
(for-each tree-get-min-greater (enumerate 10))
