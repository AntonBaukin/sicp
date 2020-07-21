(include "../2.5.1/defined.scm")
(include "../4.1.1/eval-env-tree.scm")
(include "../4.1.1/eval-env.scm")
(include "../4.1.1/eval.scm")
(include "../4.2.2/eval-lazy-includes.scm")
(include "../4.1.1/eval-basic-routine.scm")
(include "eval-lazy-streams.scm")
(include "../4.2.2/eval-lazy-cxr.scm")

; Evaluates as basic and resolves the results.
(define-macro (eval-resolve . script)
 `(basic-evaluator (list (list 'resolve ,@script)))
)
