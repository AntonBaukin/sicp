(eval-basic (debug on))

; Dispatching evaluator: «let» form, see «4.1.2-6.scm».
(assert-eq? 123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (+ a b c)
  )
 )
)

(assert-eq? -123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (set! a (- a))
   (set! b (- b))
   (set! c (- c))
   (+ a b c)
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (+ a b 1)
   )
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (let ((c (/ a (* b b))))
     (debug log-env "\n" "—— Environments of three nested lets: ")
     (+ a b c)
    )
   )
  )
 )
)

(log "\n" "Dispatching Evaluator §4.1.6 successfully tested!" "\n")
