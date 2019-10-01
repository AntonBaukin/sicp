(include "2.5.3-iterate-two.scm")

(define (log . args) (for-each display args) (newline))

(define (log-merged a b merge)
 (log
  "Merge " a  "\n"
  "With  " b  "\n"
  "  —>  " (merge-sorted a b merge)
  "\n"
 )
)


(log-merged
 '(1 2 3 4 5 7 10 11)
 '(0 2 4 6 7 8 9 10 11 12)
 (lambda (a b) (if (= a b) a (> a b)))
)

(log-merged
 '((100 . 2) (10 . 2) (0 . 1))
 '((10 . -2) (0 . 1))

 (lambda (a b)
  (cond
   ((< (car a) (car b)) #t)
   ((< (car b) (car a)) #f)
   (else (cons (car a) (+ (cdr a) (cdr b))))
  )
 )
)
