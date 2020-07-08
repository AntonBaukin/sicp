(define (log . args) (for-each display args) (newline))

(include "eval-lazy.scm")
(eval-basic (debug on))

(eval-basic
 (define count 0)

 (define (id x)
  (debug log "... inc count = " count)
  (set! count (+ count 1)) ;<— side effect
  x
 )

 (define w (id (id 10)))

 (debug log "(define w (id (id 10))) ")
 (debug log "count = " count)

 ; ... inc count = 0
 ; (define w (id (id 10)))
 ; count = 1
 ;
 ; In regular world we expect «count» to be 2, as «id()»
 ; is invoked twice. In lazy world it differs: outer call
 ; takes not a result of inner call, invoked prior, but
 ; receives a thunk that is returned, but not passed to
 ; a primary function — thus, this thunk (inner call) is
 ; never evaluated. Dumned side-effects!..
 ;

 (debug log "w = " w)

 ; ... inc count = 1
 ; w = 10
 ;
 ; Notice, that count is incremented. This is because our
 ; debug log resolves each thunk argument instead of printing
 ; it as a composite inner structure.
 ;
 ; And here «count» is expected to be 2:

 (debug log "count = " count)

 ; Dumned side-effects! We just printing a variable, «w»,
 ; and else variable, «count», is updated!
 ;
 ; Unpredictable hell...
)
