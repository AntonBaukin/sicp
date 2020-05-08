(eval-basic (debug on))

; Test error on referring anassigned variable:
(assert-error
 (lambda ()
  (eval-basic
   (define a '*unassigned*)
   (+ a 1)
  )
 )

 (lambda (message args)
  (log "—— Expected referring anassigned variable:")
  (log "Error message: " message " " (car args))
 )
)

; Test defines to let transformation:
(assert-eq? 39
 (eval-basic
  (define (f a b c)
   (define v (* a b))
   (debug log-env "\n" "—— Stack of test 39 ——")
   ; With our transformation, this works fine:
   (+ a b c v w)
   ; This definition is raised up:
   (define w (* v c))
  )

  (f 2 3 4)
 )
)

; Test definition of a function:
(assert-eq? 24
 (eval-basic
  (define (f i)
   (define (x j) (* j y (z j)))
   (define y 2)
   (x i)
   (define (z j) (+ j 1))
  )
  (f 3)
 )
)
