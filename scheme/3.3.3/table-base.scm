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
; Without takes (storage key) and removes the record.
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
(define (make-table-base make search save rewrite without length iter)
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
     (set-cdr! table (save (cdr table) key value))
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

 ; Goes by inner tables and invokes operation
 ; on the last one passing: (last-table last-key).
 ; If create? flag is given intermediate tables
 ; are created on demand; else void is returned.
 (define (do-nested create? table keys op)
  ; We have only one key left? Invoke op...
  (if (null? (cdr keys))
   (op table (car keys))
   (let ((sub (search-table table (car keys))))
    (cond
     ; Continue with found sub-table?
     ((and (pair? sub) (table? (cdr sub)))
      (do-nested create? (cdr sub) (cdr keys) op)
     )

     ((and (eq? void sub) (not create?))
      void ;<— do nothing 
     )

     ((eq? void sub) ; Sub-table is not yet created?
      (set! sub (make-table))
      (add-key-value table (car keys) sub)
      (do-nested #t sub (cdr keys) op)
     )

     (else (error "Got not a nested table by the key!" (car keys)))
    )
   )
  )
 )

 (define (nest-tables table value keys)
  (do-nested #t table keys
   (lambda (table key)
    (add-key-value table key value)
   )
  )
 )

 (define (add table value . keys)
  (nest-tables table value keys)
 )

 (define (remove-key table key)
  (let ((kv (search-table table key)))
   (if (eq? void kv) void ;<— key is absent
    (let ((old (cdr kv)))
     (set-cdr! table (without (cdr table) key))
     old ;<— return removed value
    )
   )
  )
 )

 (define (remove-nested table keys)
  (do-nested #f table keys remove-key)
 )

 (define (remove table . keys)
  (remove-nested table keys)
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
  remove         ; 4
  size           ; 5
  clear          ; 6
  iterate        ; 7
 )
)
