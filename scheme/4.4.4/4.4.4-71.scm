(include "../4.4.4/qeval-includes-std.scm")
;
; In tasks 64 and 67 Hugo Doom experimented with
; recursive rules that expand with «or»...
;
(define qeval-includes
 (append qeval-includes-std '(
  ; Toggle comment of to see the difference.
  ; In the following text we provide both results.
  ; "4.4.4-71-hugo.scm"
 ))
)

(include "../4.4.4/qeval-test-base.scm")

;
; All our previous queries were instantly translated into lists.
; Delayed execution of rules, «or» conditions has no visible
; difference when the stream is consumed ahead.
;
; To pick up this change we have to switch to iterative
; consuming of the query results and tricky inner logging...
;
(define (log-iter iter)
 (define (next i)
  (define v (iter))
  (log i ": " (if (eq? void v) "—— end ——" v))
  (if (not (eq? void v)) (next (+ i 1)))
 )

 (next 0)
)

;
; This fake predicate is used to log intermediate results
; of the query processing.
;
(define (log-head? worker head)
 (log "Is: " head " head of: " worker "?")
 #t ;<— always allow this value
)

(add-rule (subordinates ?worker ?head)
 ; Note that in task 71 Hugo removes delays from two things:
 ; 1) a query in general, 2) «or» branching.
 ;
 ; This gives us a hint: we need a rule with «or»!
 (or
  ; We swapped «and» operands to make the recursive tree
  ; of checks to advance faster. (See below.)
  (and
   (supervisor ?worker ?some)
   ; We can log the intermediate suggestions:
   (lisp-value log-head? ?worker ?some)
   ; (Normal recursion of task 64, no loops.)
   (subordinates ?some ?head)
  )
  (supervisor ?worker ?head)
 )
)

; And this is how we iterate the query:
(log-iter (query-iter
 (subordinates ?worker (Warbucks Oliver))
))

;
; Normal execution gives us the following trace:
;

; Is: (Warbucks Oliver) head of: (Aull DeWitt)?
; Is: (Scrooge Eben) head of: (Cratchet Robert)?
; Is: (Warbucks Oliver) head of: (Scrooge Eben)?
; 0: (subordinates (Cratchet Robert) (Warbucks Oliver))
; Is: (Warbucks Oliver) head of: (Scrooge Eben)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; Is: (Hacker Alyssa P) head of: (Doom Hugo)?
; Is: (Bitdiddle Ben) head of: (Hacker Alyssa P)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 1: (subordinates (Aull DeWitt) (Warbucks Oliver))
; 2: (subordinates (Doom Hugo) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Tweakit Lem E)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 3: (subordinates (Scrooge Eben) (Warbucks Oliver))
; 4: (subordinates (Tweakit Lem E) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Fect Cy D)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 5: (subordinates (Bitdiddle Ben) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Hacker Alyssa P)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 6: (subordinates (Fect Cy D) (Warbucks Oliver))
; 7: (subordinates (Hacker Alyssa P) (Warbucks Oliver))
; 8: —— end ——

;
; With the Hugo's change it's the following:
;

; Is: (Warbucks Oliver) head of: (Aull DeWitt)?
; Is: (Scrooge Eben) head of: (Cratchet Robert)?
; Is: (Warbucks Oliver) head of: (Scrooge Eben)?
; Is: (Warbucks Oliver) head of: (Scrooge Eben)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; Is: (Hacker Alyssa P) head of: (Doom Hugo)?
; Is: (Bitdiddle Ben) head of: (Hacker Alyssa P)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 0: (subordinates (Cratchet Robert) (Warbucks Oliver))
; 1: (subordinates (Aull DeWitt) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Tweakit Lem E)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 2: (subordinates (Doom Hugo) (Warbucks Oliver))
; 3: (subordinates (Scrooge Eben) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Fect Cy D)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 4: (subordinates (Tweakit Lem E) (Warbucks Oliver))
; 5: (subordinates (Bitdiddle Ben) (Warbucks Oliver))
; Is: (Bitdiddle Ben) head of: (Hacker Alyssa P)?
; Is: (Warbucks Oliver) head of: (Bitdiddle Ben)?
; 6: (subordinates (Fect Cy D) (Warbucks Oliver))
; 7: (subordinates (Hacker Alyssa P) (Warbucks Oliver))
; 8: —— end ——

;
; At the start point version without delays consumes more data.
; It may also take more data on false branches. As the evaluation
; goes, with the help of delays inside streams, the difference fades.
;
