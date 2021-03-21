;
; Producer used as (amb ?x integers) to create
; infinite stream of integers.
;
(define (integers prev)
 (if (null? prev) 1 (+ prev 1))
)

;
; This rule produces infinite stream of integers being
; diagonal sum ?c (last in the triple). Then it makes
; finite sub-streams for each one.
;
(add-rule (triple ?a ?b ?c)
 (and
  (amb ?c integers)
  (set ?a 1)
  (set ?b - ?c 1)
  (diagonal-next ?a ?b ?c)
 )
)

(add-rule (diagonal-next ?a ?b ?c)
 (and
  (lisp-value <= ?a ?b)
  (or
   (and
    ;
    ; Special form of variables referring «?:a» and «?:b»
    ; allows to assign outer values omitting unique ids.
    ;
    ; This trick makes QEval to be alike ordinary evaluator
    ; as it now able to affect current frame and produce more
    ; frames voa «or» form.
    ;
    ; Note that this has no a side-effect as frames are copied.
    ;
    (set ?:a ?a)
    (set ?:b ?b)
    (set ?:c ?c)
    (always-true)
   )
   (and
    (set ?a + ?a 1)
    (set ?b - ?b 1)
    (diagonal-next ?a ?b ?c)
   )
  )
 )
)

(define (log-iter n iter)
 (define (next i)
  (define v (iter))
  (log
   (if (< i 10) 0 "") i
   ":  "
   (if (eq? void v) "-" v)
  )
  (if (< (+ i 1) n) (next (+ i 1)))
 )

 (next 0)
)
