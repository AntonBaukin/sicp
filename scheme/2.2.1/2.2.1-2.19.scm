(define (log . args) (for-each display args) (newline))

(define (count-coins-change amount coins)
 (define (no-more? a cs)
  (or (< a 0) (null? cs))
 )

 (define (del-first cs)
  (cdr cs)
 )

 (define (get-first cs)
  (car cs)
 )

 (define (cc a cs)
  (if (= a 0) 1
   (if (no-more? a cs) 0
    (+
     (cc a (del-first cs))
     (cc (- a (get-first cs)) cs)
    )
   )
  )
 )

 (cc amount coins)
)

(define coins-us (list 50 25 10 5 1))

(define coins-uk (list 100 50 20 10 5 2 1 0.5))

(log "count 100 coins change US: " (count-coins-change 100 coins-us))
;> count 100 coins change US: 292

(log "count 100 coins change UK: " (count-coins-change 100 coins-uk))
;> count 100 coins change UK: 104561