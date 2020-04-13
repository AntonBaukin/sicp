(include "../3.3.3/tree-red-black.scm")
(include "../3.3.3/table-tree.scm")

; «Class» (collection of ops) of environment frame table.
; The implementation is based on red-black tree table from §3.3.3.
(define EvalEnvFrame
 (make-table-tree
  (lambda (a b)
   (string<? (symbol->string a) (symbol->string b))
  )
 )
)

; Creates frame to store values by symbolic names.
(define eval-env-frame-make (table-op-make EvalEnvFrame))

; Adds value to the frame: (frame value name).
(define eval-env-frame-add (table-op-add EvalEnvFrame))

; Returns value stored in the frame by the given name.
; Result void means value absence.
; Arguments: (frame name).
(define eval-env-frame-lookup (table-op-lookup EvalEnvFrame))

; Invoked with (frame iterator), where iterator takes:
; (name value) and returns void to continue iteration.
; This routine is used in the debugger.
(define eval-env-frame-iterate (table-op-iterate EvalEnvFrame))
