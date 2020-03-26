; This file is included the last, after each «eval-impl-*.scm».
; It sets the routines implementing evaluator extension points.
;

(set! eval-impl eval-basic)

(set! apply-impl apply-basic)

(set! debug-impl
 (with-exception-catcher
  (lambda (e) void)
  (lambda () debug-eval-cmd)
 )
)

(apply define-primitives eval-primes)
