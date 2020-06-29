; This file is included before the last «eval-impl-set.scm»,
; after else «eval-impl-*.scm».
;
; It adds procedures treated as «primitives», but they are just
; our language' pre-defined ones.
;
; Note: that in Gambit Scheme we can not refer special
; forms or macroses as a procedures, and we have to
; support on the evaluator level.
;
(define eval-primes
 (append
  (list
   ; This name is defined in the private scope of eval
   ; implementation. It refers the top-level environment
   ; used as the default.
   global-env
  )

  prime-ops-arithmetics
  prime-ops-comparing
  prime-ops-pairs
  prime-ops-lists
  prime-ops-checks
  prime-ops-misc
  prime-ops-cXr
 )
)
