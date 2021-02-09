;
; Implements interface to store and query the rules.
; Depends on «defs.scm».
;
(define (make-rules-db make-streams-db)
 (define db (make-streams-db))
 (define db-get (streams-db-get db))
 (define db-all (streams-db-all db))
 (define db-add (streams-db-add db))
 
 (define (rule-key rule)
  (define con (conclusion rule))
  (define key (car con))
  (if (symbol? key) key '?)
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
   (stream-append
    (db-get (pattern-key pattern))
    (db-get '?)
   )
   (db-all)
  )
 )

 (define (add rule)
  (db-add (rule-key (check-rule rule)) rule)
 )

 (list fetch add)
)
