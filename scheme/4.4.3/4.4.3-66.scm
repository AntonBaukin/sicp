(include "../3.1/accumulator.scm")
(include "../4.1.6/filter.scm")
(include "../4.4.4/qeval-test-base.scm")

(define (accumulate query-map zero mapper accumulator variable query)
 (define acc (make-accumulator zero accumulator))

 (for-each
  acc
  (map
   mapper
   (query-map
    (lambda (frame)
     (frame-get frame variable)
    )
    query
   )
  )
 )

 (acc)
)

(define (identity x) x)

(define (acc-sum sum x) (+ sum x))

(define (query-sum-impl variable query)
 (accumulate query-map 0 identity acc-sum variable query)
)

(define-macro (query-sum variable query)
 `(query-sum-impl (quote ,variable) (quote ,query))
)

(add-rule (boss ?person ?salary)
 (and
  (supervisor ?manager ?person)
  (supervisor ?x ?manager)
  (salary ?person ?salary)
 )
)

; Query from task «4.4.2-65.scm».
(log "  ————  " "The query of Fect Cy D" "  ————")
(test-and-log
 (boss ?who ?salary)
; —————————————————————————————————————————————————————————
 (boss (Warbucks Oliver) 150000)
 (boss (Warbucks Oliver) 150000)
 (boss (Bitdiddle Ben) 60000)
 (boss (Warbucks Oliver) 150000)
 (boss (Warbucks Oliver) 150000)
)

(log "  ———— " "Sum: "
 (query-sum salary (boss ?who ?salary))
)

; To solve the problem of repeating the same record,
; as may occur in SQL queries, we apply distinct
; filter to the query mapper.
(define (make-distinct-filter)
 (define items '())

 (lambda (frame)
  ; Here we create the index record from the frame.
  ; It contains the values of the top-level variables.
  ; (The order of variables in a frame is always the same.)
  (define item (clear-frame frame))
  (if (member item items) #f
   (begin
    (set! items (cons item items))
    #t
   )
  )
 )
)

; Removes from frame all bindings of special form «$i:name»
; that are created due the rule locals renaming. Distinct
; frames are checked against clean top-level variables.
; Returns the list of values.
(define (clear-frame frame)
 (define (synthetic? b)
  (eq? #\$ (string-ref (symbol->string (car b)) 0))
 )

 (filter-not
  (map
   (lambda (b)
    (if (synthetic? b) '()
     (frame-get frame (car b))
    )
   )
   (cdr frame)
  )
  null?
 )
)

(define (query-map-distinct mapper query)
 (map
  mapper
  (filter
   ; We filter distinct frames, not mapping results:
   (query-map identity query)
   (make-distinct-filter)
  )
 )
)

(define (query-sum-distinct-impl variable query)
 (accumulate query-map-distinct 0 identity acc-sum variable query)
)

(define-macro (query-sum-distinct variable query)
 `(query-sum-distinct-impl (quote ,variable) (quote ,query))
)

(log "  ———— " "Sum distinct: "
 (query-sum-distinct salary (boss ?who ?salary))
)
