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
  (define script (list 'eval-query (list 'quote query)))
  (eval-amb-results-impl (eval-amb-list-impl 100 (list script)))
 )

 ; Amb forbids void value of variables (replace it with #f):
 (define (pattern-match-safe pattern assertion frame)
  (define result (pattern-match pattern assertion frame))
  (if (eq? void result) #f result)
 )

 (define (unify-match-safe a b frame)
  (define result (unify-match a b frame))
  (if (eq? void result) #f result)
 )

 (define (eval-lisp-value call frame)
  (eval (prepare-for-eval (instantiate call frame)))
 )

 ; Utilities provided for Amb evaluator:
 (amb-eval-define 'untag untag)
 (amb-eval-define 'adb-fetch adb-fetch)
 (amb-eval-define 'rdb-fetch rdb-fetch)
 (amb-eval-define 'pattern-match pattern-match-safe)
 (amb-eval-define 'parse-query parse-query)
 (amb-eval-define 'make-pattern make-pattern)
 (amb-eval-define 'instantiate instantiate)
 (amb-eval-define 'empty-frame empty-frame)
 (amb-eval-define 'eval-lisp-value eval-lisp-value)
 (amb-eval-define 'rename-vars-in rename-vars-in)
 (amb-eval-define 'unify-match-safe unify-match-safe)
 (amb-eval-define 'rule-conclusion rule-conclusion)
 (amb-eval-define 'rule-body rule-body)
 (amb-eval-define 'next-unique-var-id next-unique-var-id)

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

 (define (apply-rules pattern frame)
  (define rule (amb-of-stream (rdb-fetch pattern frame)))
  (apply-rule rule pattern frame)
 )

 (define (apply-rule rule pattern frame)
  (define unique-rule (rename-vars-in next-unique-var-id rule))

  (define match-frame
   (unify-match-safe
    (untag pattern)
    (rule-conclusion unique-rule)
    frame
   )
  )

  (require (pair? match-frame))

  (qeval-disp
   (make-pattern (rule-body unique-rule))
   match-frame
  )
 )

 (define (simple-query pattern frame)
  (amb
   (find-assertions pattern frame)
   (apply-rules pattern frame)
  )
 )

 ; In this singlestanding task, we do not make Amb QEval modular.
 ; Note the main difference with streamed QEval: Amb version
 ; with single frame, not a stream of frames.
 ; This due the nature of Amb.
 (define (qeval-disp pattern frame)
  (define query (untag pattern))
  (define form (car query))

  (cond
   ((eq? form 'and)
    (qeval-and (cdr query) frame)
   )

   ((eq? form 'or)
    (qeval-or (cdr query) frame)
   )

   ((eq? form 'not)
    (qeval-not (cdr query) frame)
   )

   ((eq? form 'lisp-value)
    (qeval-value (cdr query) frame)
   )

   ((eq? form 'always-true)
    (always-true '() frame)
   )

   (else
    (simple-query pattern frame)
   )
  )
 )

 (global qeval-disp)

 (define (eval-query query)
  (define parsed-query (parse-query query))
  (define pattern (make-pattern parsed-query))
  (define frame (qeval-disp pattern empty-frame))

  (instantiate parsed-query frame)
 )

 (global eval-query)
)
