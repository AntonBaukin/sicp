
(define (inverter in out)
 (define (trigger w)
  (define d (agenda-param out 'inverter-delay))
  (define s (not (get-signal in)))
  (after-delay out d (lambda (a) (set-signal out s)))
 )

 (on-wire in trigger)

 (list
  'inverter
  (get-agenda out)
  trigger
 )
)

(define (and-gate in0 in1 out)
 (define (trigger w)
  (define d (agenda-param out 'and-gate-delay))
  (define s (and (get-signal in0) (get-signal in1)))
  (after-delay out d (lambda (a) (set-signal out s)))
 )

 (on-wire in0 trigger)
 (on-wire in1 trigger)

 (list
  'and-gate
  (get-agenda out)
  trigger
 )
)

(define (or-gate in0 in1 out)
 (define (trigger w)
  (define d (agenda-param out 'or-gate-delay))
  (define s (or (get-signal in0) (get-signal in1)))
  (after-delay out d (lambda (a) (set-signal out s)))
 )

 (on-wire in0 trigger)
 (on-wire in1 trigger)

 (list
  'or-gate
  (get-agenda out)
  trigger
 )
)

(define (probe wire name)
 (on-wire wire
  (lambda (w)
   (log
    "[" name "] " "@" (get-time wire)
    " ~> " (if (get-signal wire) "1" "0")
   )
  )
 )
)
