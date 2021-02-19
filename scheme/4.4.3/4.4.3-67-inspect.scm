; Depends on "../3.3.3/table.scm".

;
; Implementation of loop detector for QEval protection.
;
(define (make-qeval-loop-detector max-stack-depth max-repeat-count)
 (define (map-bindings result bindings)
  (if (null? bindings) result
   (let ((x (map-binding (car bindings))))
    (map-bindings
     (if (null? x) result (cons x result))
     (cdr bindings)
    )
   )
  )
 )

 (define (map-binding b)
  (log "X> " (binding-value b))

  (if (variable? (binding-value b))
   (map-reference
    (binding-name b)
    (variable-name (binding-value b))
   )
   '()
  )
 )

 (define (map-reference source target)
  (define sname (decode-unique-var source))
  (define tname (decode-unique-var target))

  (if (or (null? sname) (null? tname))
   '()
   (cons sname tname)
  )
 )

 (define (make-entry query frame-stream)
  (if (stream-null? frame-stream)
   (list query empty-frame '())
   (list
    query
    (stream-car frame-stream)
    (map-bindings '() (frame-bindings (stream-car frame-stream)))
   )
  )
 )

 (define (max-stack? stack)
  (define result (> (length stack) max-stack-depth))

  (if result
   (log "QEval ERROR: reached maximum evaluation stack depth")
  )

  result
 )

 (define open-loop-detector (make-open-loop-detector max-repeat-count))

 (define (open-loop? entry)
  (define bindings (list-ref entry 2))
  (define check (open-loop-detector bindings))

  (if check
   (log "QEval ERROR: detected open loop with rule variables pair «" (car check) "»")
  )

  (not (eq? #f check))
 )

 (define (looped? entry stack)
  (or
   (max-stack? stack)
   (open-loop? entry)
  )
 )

 (cons make-entry looped?)
)

;
; Creates strategy to test rule variables dependency entries
; of form ((a . N) . (b . M)) upon open infinite loop.
;
; Item (a . N) means rule variable «a» with unique index «N».
; Pair ((a . N) . (b . M)) means that «a» is resolved into «b»
; (where N < M) of nested rule invocation.
;
; Open loop looks like list of ((a . 1) . (b . 2)) ((a . 2) . (b . 3))
; ((a . 3) . (b . 4)) ... items. We count the stack distances of (2 - 1)
; (3 - 2) (4 - 3) ... and compare them against the strategy threshold.
;
; Result #f means no loop. On a loop the result is the list of
; ("n:m" count (list of (N . M))).
;
(define (make-open-loop-detector max-repeat-count)
 (define IndexTable (make-table equal?))
 (define make-index (table-op-make IndexTable))
 (define index-lookup (table-op-lookup IndexTable))
 (define index-add (table-op-add IndexTable))
 (define index-iter (table-op-iterate IndexTable))

 (define (var-name->string name)
  (cond
   ((string? name) name)
   ((symbol? name) (symbol->string name))
   (else (error "Wrong variable name type" name))
  )
 )

 ; Converts ((a . N) . (b . M)) into ("a:b" . (N . M)).
 (define (index-binding-item b)
  (cons
   (string-append
    (var-name->string (caar b))
    ":"
    (var-name->string (cadr b))
   )
   (cons (cdar b) (cddr b))
  )
 )

 (define (make-index-item key nm)
  (cons key (list nm))
 )

 (define (add-index-item e nm)
  (set-cdr! e (cons nm (cdr e)))
 )

 (define (index-binding index b)
  (define item (index-binding-item b))
  (define key (car item))
  (define nm (cdr item))
  (define e (index-lookup index key))

  (if (eq? void e)
   (index-add index (make-index-item key nm) key)
   (add-index-item e nm)
  )
 )

 (define (check-index index)
  (define check #f)

  (index-iter index
   (lambda (_ item)
    (define count (looped-index-item? item))

    (if (> count 0)
     (begin
      (set! check (list (car item) count (cdr item)))
      #f  ;<— break the iteration
     )
     void ;<— continue the iteration
    )
   )
  )

  check ;<— check results
 )

 ; Checks list of (N . M) pairs on looped sequence.
 ; Returns 0 on test pass, or integer that exeded
 ; tolerance threshold of «max-repeat-count».
 (define (looped-index-item? item)
  (define nms (cdr item))
  ; Table that maps (nm-delta -> count):
  (define ix (make-index))
  (define result 0)

  ; Add deltas to special index.
  (for-each
   (lambda (nm)
    (define d (- (cdr nm) (car nm)))
    (define c (index-lookup ix d))
    (index-add ix (if (eq? void c) 1 (+ c 1)) d)
   )
   (cdr item)
  )

  (index-iter ix
   (lambda (_ count)
    (if (> count max-repeat-count)
     (begin
      (set! result count)
      #f  ;<— break the iteration
     )
     void ;<— continue the iteration
    )
   )
  )

  result
 )

 (lambda (bindings)
  (define index (make-index))
  (log "  >> " bindings)
  (for-each (curry index-binding index) bindings)
  (check-index index)
 )
)
