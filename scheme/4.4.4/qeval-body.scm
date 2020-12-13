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

(define (make-pattern-matcher pattern input-frame)
 (define query (untag (check-pattern pattern)))

 (lambda (assertion)
  (define statement (untag assertion))
  (define matched-frame (pattern-match query statement input-frame))
  (if (eq? void matched-frame) #f matched-frame)
 )
)

(define (fetch-assertions pattern frame)
 (stream-filter
  (make-pattern-matcher pattern frame)
  (adb-fetch pattern frame)
 )
)

(define (simple-query pattern frame-stream)
 (stream-flatmap
  (lambda (frame)
   ; TODO: extend query with rules matching
   (fetch-assertions pattern frame)
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

(define (make-qeval-body)
 (list add-statement rdb-add eval-query)
)
