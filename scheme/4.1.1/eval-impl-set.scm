; This file is included the last, after each «eval-impl-*.scm».
; It sets the routines implementing evaluator extension points.
;

(set! eval-impl eval-basic)

(set! apply-impl apply-basic)

(set! debug-impl debug-eval-cmd)

(apply define-variables eval-primes)
