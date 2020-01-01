
(define (probe name connector)
 (define (check c)
  (if (eq? c connector) c
   (error "Wrong connector signals probe!" name)
  )
 )

 (define (on-value c)
  (log "Probe [" name "] ~=> " (connector-get-value (check c)))
 )

 (define (on-reset c)
  (check c)
  (log "Probe [" name "] ~=> Ø")
 )

 (define me (list 'probe on-value on-reset name connector))

 (connect connector me)
 me ;<— resulting instance
)


(define (make-biop TAG make-on-value)
 (lambda (a b s)
  (define me (list TAG))

  (define (set connector value)
   (connector-set-value connector value me)
  )

  (define on-value (make-on-value me set a b s))

  (define (on-reset c)
   (reset-connectors me a b s)
   (on-value c)
  )

  ; Attach state to «this» instance:
  (set-cdr! me (list on-value on-reset a b s))

  ; Connect this constraint to income connectors:
  (connect-each me a b s)

  me ;<— resulting instance
 )
)

(define adder ((lambda ()
 (define get connector-get-value)

 (define (has-values? x y)
  (and
   (connector-has-value? x)
   (connector-has-value? y)
  )
 )

 (define (make-on-value me set a b s)
  (lambda (c)
   (cond
    ((has-values? a b)
     (set s (+ (get a) (get b)))
    )

    ((has-values? a s)
     (set b (- (get s) (get a)))
    )

    ((has-values? b s)
     (set a (- (get s) (get b)))
    )
   )
  )
 )

 (make-biop 'adder make-on-value)
)))


(define multiplier ((lambda ()
 (define get connector-get-value)

 (define (zero? x)
  (and
   (connector-has-value? x)
   (= 0 (get x))
  )
 )

 (define (has-values? x y)
  (and
   (connector-has-value? x)
   (connector-has-value? y)
  )
 )

 (define (make-on-value me set a b p)
  (lambda (c)
   (cond
    ((or (zero? a) (zero? b))
     (set p 0)
    )

    ((has-values? a b)
     (set p (* (get a) (get b)))
    )

    ((has-values? a p)
     (set b (/ (get p) (get a)))
    )

    ((has-values? b p)
     (set a (/ (get p) (get b)))
    )
   )
  )
 )

 (make-biop 'multiplier make-on-value)
)))


(define (constant value connector)
 (define me (list 'constant))

 (define (on-error c)
  (error "Constant connector may not be altered!")
 )

 ; Attach state to «this» instance:
 (set-cdr! me (list on-error on-error value connector))

 (connect connector me)

 ; Set connector value right now:
 (connector-set-value connector value me)

 me ;<— resulting instance
)

(define (constraint-noop v) void)
