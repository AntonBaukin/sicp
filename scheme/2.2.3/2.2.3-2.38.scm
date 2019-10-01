(define (log . args) (for-each display args) (newline))

; operation is (op v acc)
(define (fold-right sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

; operation is (op acc v) — not reversed arguments!
(define (fold-left sequence initial op)
 (define (iter res tail)
  (if (null? tail) res
   (iter (op res (car tail)) (cdr tail))
  )
 )

 (iter initial sequence)
)

(define (test-folds initial op-what op sequence)
 (log "fold right of " sequence " from  " initial "  with  "
  op-what "  gives: " (fold-right sequence initial op))

 (log "fold  left of " sequence " from  " initial "  with  "
  op-what "  gives: " (fold-left sequence initial op))
)

(test-folds 1 "/" / (list 1 2 3))

(test-folds (list) "(list ...)" list (list 1 2 3))
