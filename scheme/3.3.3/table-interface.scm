; Interface operations of a table.

; Creates empty table of the given table ops set.
; Has no arguments.
(define (table-op-make table-ops)
 (list-ref table-ops 0)
)

; Answers whether the given reference is an instance
; of table of the same table-ops «class».
; Has single argument: (table).
(define (table-op-table? table-ops)
 (list-ref table-ops 1)
)

; For the given table searches the value by the given keys.
; If more that one key is given, treates leading keys as
; the keys of the nested tables.
; Arguments: (table key-0 ...).
(define (table-op-lookup table-ops)
 (list-ref table-ops 2)
)

; Adds the given value to the table with support for
; the nested tables that are created on-demand.
; Arguments: (table value key-0 ...).
(define (table-op-add table-ops)
 (list-ref table-ops 3)
)

; Returns the size of the table.
; Has single argument: (table).
(define (table-op-size table-ops)
 (list-ref table-ops 4)
)

; Clears the table and returns it back.
; Has single argument: (table).
(define (table-op-clear table-ops)
 (list-ref table-ops 5)
)

; Iterates over the table.
; Arguments: (table visitor), where visitor takes
; Visitor takes two arguments: key, and value.
; You may change the value returning not #f or void.
; Returning #f you break the iteration.
(define (table-op-iterate table-ops)
 (list-ref table-ops 6)
)
