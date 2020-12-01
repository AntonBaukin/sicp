
(define (make-qeval-body)
 (define adb (make-assertions-db make-streams-db))
 (define adb-add (assertions-db-add adb))
 (define adb-fetch (assertions-db-fetch adb))

 (define rdb (make-rules-db make-streams-db))
 (define rdb-add (rules-db-add rdb))
 (define rdb-fetch (rules-db-fetch rdb))

 (define empty-frame (make-frame '()))

 (define (add-statement statement)
  (adb-add (make-assertion statement))
 )

 (define (make-pattern-matcher pattern)
  (define query (untag pattern))

  (lambda (assertion)
   (define statement (untag assertion))
   (define frame (pattern-match query statement empty-frame))
   (if (eq? void frame) #f frame)
  )
 )

 (define (fetch-assertions pattern)
  (stream-filter
   (make-pattern-matcher pattern)
   (adb-fetch pattern empty-frame)
  )
 )

 (define (eval-query-stream pattern)
  ; TODO: extend query with rules matching
  (fetch-assertions pattern)
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

 (list add-statement rdb-add eval-query)
)
