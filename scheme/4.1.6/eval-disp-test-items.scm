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

; Dispatching evaluator: «and» form.
(assert-true?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 2))
 )
)

(assert-false?
 (eval-basic
  (and (= 1 2) (error "Wrong!"))
 )
)

; Dispatching evaluator: «or» form.
(assert-true?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 2))
 )
)

(assert-true?
 (eval-basic
  (or (= 1 1) (error "Wrong!"))
 )
)

(log "\n" "Dispatching Evaluator §4.1.6 successfully tested!" "\n")
