(define (log . args) (for-each display args) (newline))

(define (make-monitored f)
 (define counter 0)

 (define (asked-cmd? args cmd-symbol)
  (and
   (= 1 (length args))
   (eq? cmd-symbol (car args))
  )
 )

 ; Essentially, we support arbitrary number of arguments.
 (define (monitor . args)
  (cond
   ((asked-cmd? args 'how-many-calls?)
    counter
   )

   ((asked-cmd? args 'reset-count!)
    (set! counter 0)
    0 ;<— and return 0
   )

   (else
    (set! counter (+ counter 1))
    (apply f args)
   )
  )
 )

 monitor
)

; And now we rewrite our custom sqrt() from «1.1.7-sqrt.3.scm».
(define (sqrt-next y x)
 (if (< (abs (- (* y y) x)) 0.001) y
  (sqrt-next (* 0.5 (+ y (/ x y))) x)
 )
)

(define (sqrt x)
 (sqrt-next 1.0 x)
)

; Make sqrt-next() monitored!
(set! sqrt-next (make-monitored sqrt-next))

(define (log-sqrt x)
 ; First, we reset the counter
 (sqrt-next 'reset-count!)
 (log "√" x " = " (sqrt x) " in " (sqrt-next 'how-many-calls?) " steps")
)

(log-sqrt 121)
(log-sqrt 152399025)
