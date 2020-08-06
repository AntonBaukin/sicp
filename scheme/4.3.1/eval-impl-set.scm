; This file is included the last, after each «eval-impl-*.scm».
; It sets the routines implementing evaluator extension points.
;

; Use our nondeterministic evaluator:
(set! eval-impl eval-amb)

(set! apply-impl apply-amb)

(set! debug-impl
 (with-exception-catcher
  (lambda (e) void)
  (lambda () debug-eval-cmd)
 )
)

(apply define-primitives eval-primes)

; Here we register basic forms for the dispatching evaluator:
(apply eval-disp-register eval-disp-basic-forms)
