(include "random.scm")
(include "../2.3.3/tree.scm")
(include "../2.5.3/index-tree.scm")

(define (log . args) (for-each display args) (newline))

; Generates N integers and returns 0..M histogram index tree.
(define (run-tests m n)
 (define hist (make-index-tree))

 (define (get i) ((car hist) i))
 (define (set i v) ((cadr hist) i v))

 (define (inc i)
  (let ((v (get i)))
   (set i (if (null? v) 1 (+ v 1)))
  )
 )

 (define (test run)
  (if (> run n) hist
   (let ((i (modulo (truncate (rand)) m)))
    (inc i)
    (test (+ run 1))
   )
  )
 )

 (test 0)
)

(define (hist->str hist)
 (define s "")

 ((index-tree-iter hist)
  (lambda (k v)
   (set! s
    (string-append s
     (if (= 0 (string-length s)) "" ", ")
     (number->string k) ": "
     (number->string v)
    )
   )
   void
  )
 )

 s
)

(define (run-and-log m n)
 (define hist (run-tests m n))
 (log "Run " n " tests, histogram:\n" (hist->str hist) "\n")
)

(define rand (make-random 1))
(run-and-log 2 1000)
(run-and-log 10 10000)

(set! rand (make-random-in-range rand 5 10))
(run-and-log 11 10000) ;<— 11 is mod-safe for [5 .. 10]
