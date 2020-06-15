(eval-basic (debug on))

; Pure recursive version of Fibonacci evaluation
; that causes huge branching and massive calls.
(eval-basic
 (define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else
    (+
     (fib (- n 2))
     (fib (- n 1))
    )
   )
  )
 )

 (global fib-rec fib)
)

(reset-ts)
(assert-eq? 17711 (eval-basic (fib-rec 22)))
(log "  pure recursive Fibonacci [22] took: " (ts) " sec")


; Co-recursive version of Fibonacci evaluation.
(eval-basic
 (define (fib n)
  (define (next i a b)
   (if (= i n)
    b
    (next (+ i 1) b (+ a b))
   )
  )

  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (next 1 0 1))
  )
 )

 (global fib-co fib)
)

(reset-ts)
(eval-basic (fib-co 10000))
(log "  co-recursive Fibonacci [10k] took: " (ts) " sec")


(define LIST_10k
 (
  (lambda () ;<— immediately invoked function
   (define N 10000)

   (define (next i res)
    (if (= N i) res (next (+ i 1) (cons i res)))
   )

   (next 0 '())
  )
 )
)

(basic-eval-define 'LIST_10k LIST_10k)

; Co-recursive reverse.
(eval-basic
 (define (reverse l)
  (define (next res tail)
   (if (null? tail) res
    (next (cons (car tail) res) (cdr tail))
   )
  )

  (next '() l)
 )

 (global rev-co reverse)
)

(reset-ts)
(eval-basic (rev-co LIST_10k))
(log "  reverse of 10k list: " (ts) " sec")
