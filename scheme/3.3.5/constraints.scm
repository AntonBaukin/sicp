
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

(define (adder a b s)
 (define me (list 'adder))

 (define (has-values? x y)
  (and
   (connector-has-value? x)
   (connector-has-value? y)
  )
 )

 (define get connector-get-value)

 (define (set c value)
  (connector-set-value c value me)
 )

 (define (on-value c)
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
