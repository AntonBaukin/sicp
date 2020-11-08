;
; Implements interface to store and query the assertions.
; Directly depends on «defs.scm».
;
(define (make-assertions-db make-streams-db)
 (define db (make-streams-db))
 (define db-get (streams-db-get db))
 (define db-all (streams-db-all db))
 (define db-add (streams-db-add db))

 (define (assertion-key assertion)
  (car (untag assertion))
 )

 (define (pattern-key pattern)
  (car (untag pattern))
 )

 (define (use-index? pattern)
  (constant-symbol? (pattern-key pattern))
 )

 (define (fetch pattern frame)
  (check-pattern pattern)
  (check-frame frame)

  (if (use-index? pattern)
   (db-get (pattern-key pattern))
   (db-all)
  )
 )

 (define (add assertion)
  (check-assertion assertion)
  (db-add (assertion-key assertion) assertion)
 )

 (list fetch add)
)
