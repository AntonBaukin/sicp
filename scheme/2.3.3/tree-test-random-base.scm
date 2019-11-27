(include "tree-test-random-run.scm")


(define (delete-item lst index)
 (define (next i head tail)
  (if (= i index)
   (append (reverse head) (cdr tail))
   (next (+ i 1) (cons (car tail) head) (cdr tail))
  )
 )

 (next 0 '() lst)
)

(define (delete-value lst value)
 (define (next head tail)
  (if (eq? (car tail) value)
   (append (reverse head) (cdr tail))
   (next (cons (car tail) head) (cdr tail))
  )
 )

 (next '() lst)
)

(define (run-test test index)
 ; Target test tree:
 (define tree '())
 (define (add num) (set! tree (num-tree-add tree num)))

 (define (gen-test index n source)
  ; Add all items to form random tree:
  (for-each add source)

  ; Are the same sorted items produced?
  (assert-equal?
   (num-sort source)
   (num-tree->list tree)

   (lambda (sorted result)
    (log "\n"
     "Seed: " seed "\n"
     "Index: " index "\n"
     "Source: " source "\n"
     "Sorted: " sorted "\n"
     "Result: " result "\n"
     (num-tree->str tree)
    )

    (error "Tree add failed!")
   )
  )

  (test index n source tree add)
 )

 (run-test-gen gen-test index)
)

(define (run-test-cycles T test)
 (map (curry run-test test) (enumerate-n T))
 (log "Successfully completed tests: " T)
)
