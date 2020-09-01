(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")
(include "eval-amb.scm")

(eval-basic (debug on))
;
; So, to check Ben's proposal we create two versions
; and calculate the number of «require» checks.
; We also provide general considerations.
;
(eval-basic
 (define (integer-between a b)
  (require (<= a b))
  (amb a (integer-between (+ a 1) b))
 )

 (define (pythagorean-triples-a n)
  (let ((i (integer-between 1 n)))
   (let ((j (integer-between i n)))
    (let ((k (integer-between j n)))
     (debug inc require)
     (require (= (+ (* i i) (* j j)) (* k k)))
     (list i j k)
    )
   )
  )
 )

 (global pythagorean-triples-a)

 ;<— Ben's version of the same function:
 (define (pythagorean-triples-b n)
  (define hsq (* n n))
  (let ((i (integer-between 1 n)))
   (let ((j (integer-between i n)))
    (let ((ksq (+ (* i i) (* j j))))
     (debug inc require)
     ; Note that we have replaced two require tests with signle:
     (require (and (>= hsq ksq) (integer? (sqrt ksq))))
     (list i j (sqrt ksq))
    )
   )
  )
 )

 (global pythagorean-triples-b)
)

(log "——— Compare Pythagorean triples find routines ———")
(reset-ts)

(log (eval-amb-results (pythagorean-triples-a 20)))
(log "  normal variant time: " (ts))
(log "  tests counter: " (eval-basic (debug get require)))

(log "\n")
(eval-basic (debug del require))
(reset-ts)

(log (eval-amb-results (pythagorean-triples-b 20)))
(log "  Ben's variant time: " (ts))
(log "  tests counter: " (eval-basic (debug get require)))
;
; Found triples are exactly the same:
; :> ((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))
;
; Here are the stats:
;
;  normal variant time: 1.768
;  tests counter: 1540
;
;  Ben's variant time: 0.241
;  tests counter: 210
;
; Ben's variant reduces tests in 7 times, and so the time.
; Lets find out why this is so...
;
; If we run test «a» for «n» in 1..20, the tests counters are:
;  {15, 680}, {18, 1140}, {19, 1330}, {20, 1540}
;
; You may use these items to approximate function in Wolfram:
;   interpolating polynomial |  {15, 680}, {18, 1140}, ...
;
; It gives exact match: Counts-a(n) = (n³ + 3n² + 2n) / 6
;
; Here are counters for Ben's test «b»:
;  {15, 120}, {18, 171}, {19, 190}, {20, 210}
;
; It gives exact match: Counts-b(n) = (n² + n) / 2
;
; Each loop like i..n adds o(n) multiplier to the counts.
; Initial variant used three cycles, thus got o(n³).
;
; Ben used two cycles and involved float point computations,
; but as he has o(n²), this has no matter.
;
