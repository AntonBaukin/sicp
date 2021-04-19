;
; Actual implementation of the evaluator.
;
; Note: that instances of assertion and rule databases
; are global, and not included into «make-qeval-body»
; that becomes trivial. This is valid because all the
; modules are included in local scope of QEval maker.
; So, all included stuff is already privite, but it
; shares the same scope, and well-known names may
; be used to simplify cross-dependences.
;

(define adb (make-assertions-db make-streams-db))
(define adb-add (assertions-db-add adb))
(define adb-fetch (assertions-db-fetch adb))

(define rdb (make-rules-db make-streams-db))
(define rdb-add (rules-db-add rdb))
(define rdb-fetch (rules-db-fetch rdb))

(define empty-frame-stream (singleton-stream empty-frame))

(define (add-statement statement)
 (adb-add (make-assertion statement))
)

(define (add-rule rule)
 (rdb-add (parse-query rule))
)

(define (make-pattern-matcher pattern input-frame)
 (define query (untag (check-pattern pattern)))

 (lambda (assertion)
  (define statement (untag assertion))
  (define matched-frame (pattern-match query statement input-frame))
  (if (eq? void matched-frame) #f matched-frame)
 )
)

(define (find-assertions pattern frame)
 (stream-filter
  (make-pattern-matcher pattern frame)
  (adb-fetch pattern frame)
 )
)

(define (simple-query pattern frame-stream)
 (stream-flatmap
  (lambda (frame)
   (stream-append-delayed
    (find-assertions pattern frame)
    (delay (apply-rules pattern frame))
   )
  )
  frame-stream
 )
)

(define (eval-query-stream pattern)
 (qeval-disp pattern empty-frame-stream)
)

(define (eval-query query)
 (define parsed-query (parse-query query))
 (define pattern (make-pattern parsed-query))

 (stream->list
  (stream-map
   (curry instantiate parsed-query)
   (eval-query-stream pattern)
  )
 )
)

(define (eval-query-map mapper query)
 (define parsed-query (parse-query query))
 (define pattern (make-pattern parsed-query))

 (stream->list
  (stream-map
   mapper
   (eval-query-stream pattern)
  )
 )
)

(define (eval-query-iter query)
 (define parsed-query (parse-query query))
 (define pattern (make-pattern parsed-query))

 (stream->iterator
  (stream-map
   (curry instantiate parsed-query)
   (eval-query-stream pattern)
  )
 )
)

(define (make-qeval-body)
 (list
  add-statement
  add-rule
  eval-query
  eval-query-map
  eval-query-iter
 )
)

(define (apply-rules pattern frame)
 (stream-flatmap
  (lambda (rule)
   (apply-rule rule pattern frame)
  )
  (rdb-fetch pattern frame)
 )
)

(define (apply-rule rule pattern frame)
 (define unique-rule (rename-vars-in next-unique-var-id rule))

 ; Note that heavy unify-match-resolved() resolves values
 ; of variables that depend on other variables. Here we
 ; do not apply it, but move to recursive instantiate().
 (define match-frame
  (unify-match
   (untag pattern)
   (rule-conclusion unique-rule)
   frame
  )
 )

 (if (eq? void match-frame)
  the-empty-stream
  (qeval-disp
   (make-pattern (rule-body unique-rule))
   (singleton-stream match-frame)
  )
 )
)
