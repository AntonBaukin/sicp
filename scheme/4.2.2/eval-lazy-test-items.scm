(eval-basic (debug on))

; Lazy evaluator: test lazy argument (comes from SICP).
(assert-eq? 1
 (eval-basic
  (define (try a b)
   (if (= a 0) 1 b)
  )

  ; In normal evaluator second argument is resolved before
  ; invoking function «try» what causes an error.
  (try 0 (/ 1 0))
 )
)

(log "\n" "Lazy Analyzing Evaluator §4.2.2 successfully tested!" "\n")
