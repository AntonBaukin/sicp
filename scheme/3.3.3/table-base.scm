(include "table-interface.scm")

; Implementation base of a table that relies upon
; ops set independent on the storage implementation.
;
; Make takes no arguments and returns storage instance.
;
; Search takes (storage key) and returns (key . value)
; pair, or void on abcense.
;
; Save is invokedonly on abcense of the key. It takes:
; (storage key value) and returns new storage.
;
; Rewrite takes (storage (key . value) new-value), where
; (key . value) pair was previously returned by search.
; Rewrite returns new storage.
;
; Note: we split save-rewrite to allow directly update
; storage (key . value) pair without altering storage.
;
; Length takes (storage) and returns it's size.
;
; Iter takes (storage visitor), where visitor takes:
; (key value) and returns one of: #f — to break;
; void — to continue; else — the new value to assign.
;
(define (make-table-base make search save rewrite length iter)
 ; Hidden marker of a table.
 (define (marker) void)

 (define (make-table)
  (cons marker (make))
 )

 ; Table uses hidden marker instead of
 ; a public available symbol like 'table.
 (define (table? x)
  (and (pair? x) (eq? marker (car x)))
 )

 (define (search-table table key)
  (if (table? table)
   (search (cdr table) key)
   (error "Search key not in a table!")
  )
 )

 (define (search-all where keys)
  (if (null? keys) where
   (search-all
    (let ((sub (search-table where (car keys))))
     (if (or (null? (cdr keys)) (not (pair? sub)))
      sub (cdr sub)
     )
    )
    (cdr keys)
   )
  )
 )

 (define (lookup table . keys)
  (if (null? keys)
   (error "There are no table lookup keys!")
   (let ((kv (search-all table keys)))
    (if (eq? void kv) void (cdr kv))
   )
  )
 )

 (define (add-key-value table key value)
  (let ((kv (search-table table key)))
   (if (eq? void kv) ;<— key is absent?
    (begin
     ; Add new (key . value) pair to the table list:
     (set-cdr! table (save table key value))
     void ;<— return absent previous value
    )
    (let ((old (cdr kv)))
     ; Just assign the new value to existing pair:
     (set-cdr! table (rewrite (cdr table) kv value))
     old ;<— return old (overwritten) value
    )
   )
  )
 )

 (define (nest-tables table value keys)
  ; We have only one key left? Insert the value
  (if (null? (cdr keys))
   (add-key-value table (car keys) value)
   (let ((sub (search-table table (car keys))))
    (cond
     ; Continue with found sub-table?
     ((and (pair? sub) (table? (cdr sub)))
      (nest-tables (cdr sub) value (cdr keys))
     )

     ((eq? void sub) ; Sub-table is not yet created?
      (set! sub (make-table))
      (add-key-value table (car keys) sub)
      (nest-tables sub value (cdr keys))
     )

     (else (error "Got not a nested table by the key!" (car keys)))
    )
   )
  )
 )

 (define (add table value . keys)
  (nest-tables table value keys)
 )

 (define (size table)
  (if (table? table)
   (length (cdr table))
   (error "Not a table to get it's size" table)
  )
 )

 (define (clear table)
  (if (table? table)
   (begin
    (set-cdr! table (make))
    table
   )
   (error "Can not clear not a table!" table)
  )
 )

 (define (iterate table visitor)
  (if (not (table? table))
   (error "May not iterate not a table!" table)
   (iter (cdr table) visitor)
  )
 )


 (list ;<— resulting list of the ops
  make-table     ; 0
  table?         ; 1
  lookup         ; 2
  add            ; 3
  size           ; 4
  clear          ; 5
  iterate        ; 6
 )
)
