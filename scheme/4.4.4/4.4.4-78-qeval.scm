;
; Refer «qeval.scm» for the comments that are not copied here.
;

(define (qeval-add qeval)
 (list-ref qeval 0)
)

(define (qeval-rule qeval)
 (list-ref qeval 1)
)

(define (qeval-query qeval)
 (list-ref qeval 2)
)

(define (qeval-add-statements qeval statements)
 (for-each (qeval-add qeval) statements)
)

(define (make-qeval-amb)
 (include "interfaces.scm")
 (include "defs.scm")
 (include "utilities.scm")
 (include "matching.scm")
 (include "streams-db.scm")
 (include "assertions.scm")
 (include "rules.scm")

 (define adb (make-assertions-db make-streams-db))
 (define adb-add (assertions-db-add adb))
 (define adb-fetch (assertions-db-fetch adb))

 (define rdb (make-rules-db make-streams-db))
 (define rdb-add (rules-db-add rdb))
 (define rdb-fetch (rules-db-fetch rdb))

 (define (add-statement statement)
  (adb-add (make-assertion statement))
 )

 (define (add-rule rule)
  (rdb-add (parse-query rule))
 )

 (define (eval-query query)
  (define script (list 'eval-query query))
  (eval-amb-result-impl script)
 )

 ; Utilities provided for Amb evaluator:
 (amb-eval-define 'untag untag)
 (amb-eval-define 'adb-fetch adb-fetch)
 (amb-eval-define 'pattern-match pattern-match)
 (amb-eval-define 'parse-query parse-query)
 (amb-eval-define 'make-pattern make-pattern)
 (amb-eval-define 'instantiate instantiate)
 (amb-eval-define 'empty-frame empty-frame)

 (list add-statement add-rule eval-query)
)

(eval-basic
 (define (find-assertions pattern frame)
  (define query (untag pattern))
  (define assertions-stream (adb-fetch pattern frame))
  (define assertion (amb-of-stream assertions-stream))
  (define statement (untag assertion))
  (define matched-frame (pattern-match query statement frame))

  (require (pair? matched-frame))
  matched-frame ;<— return matched frame of amb branch
 )

 (define (simple-query pattern frame)
  (find-assertions pattern frame)
 )

 (define (eval-query query)
  (define parsed-query (parse-query query))
  (define pattern (make-pattern parsed-query))
  (define frame (simple-query pattern empty-frame))

  (instantiate parsed-query frame)
 )

 (global eval-query)
)
