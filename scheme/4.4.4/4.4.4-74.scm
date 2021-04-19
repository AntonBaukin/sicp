(include "../4.4.4/qeval-includes-std.scm")

(define qeval-includes
 (append qeval-includes-std '("4.4.4-74-setup.scm"))
)

; Do cover tests. It passes exactly the same assertions
; as the default implementation. But these tests are finite.
;
; Alyssa made the same changes as task 4.72 does: simple
; flat map is synchronous, but interleave was delayed.
;
; However, changes of «not» and «lisp-value» forms, and
; «find-assertions» query support produce one or no frames,
; thus do not affect the evaluation branching.
;
(include "qeval-test.scm")
