(include "table-interface.scm")
(include "../3.3.2/iterate.scm")

; Table is a list of pairs (key . value).
; To compare keys on equality keys-eq? is used.
(define (make-table keys-eq?)
 ; Hidden marker of a table.
 (define (marker) void)

 (define (make-table)
  (cons marker '())
 )

 ; Table uses hidden marker instead of
 ; a public available symbol like 'table.
 (define (table? x)
  (and (pair? x) (eq? marker (car x)))
 )

 (define (search tail key)
  (if (null? tail) void
   (if (keys-eq? key (caar tail))
    (car tail) ;<— return (key . value)
    (search (cdr tail) key)
   )
  )
 )

 (define (search-table table key)
  (if (table? table)
   (search (cdr table) key)
   (error "Search key in not a table")
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
   (error "There are no table lookup keys")
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
     (set-cdr! table (cons (cons key value) (cdr table)))
     void ;<— return absent previous value
    )
    (let ((old (cdr kv)))
     ; Just assign the new value to existing pair:
     (set-cdr! kv value)
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

     (else (error "Got not a nested table by the key" (car keys)))
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
    (set-cdr! table '())
    table
   )
   (error "Can not clear not a table" table)
  )
 )

 (define (iterate table visitor)
  (if (not (table? table))
   (error "May not iterate on not a table" table)
   (iterate-list (cdr table)
    (lambda (kv)
     (let ((res (visitor (car kv) (cdr kv))))
      (cond
       ((eq? #f res) #f) ;<— do break
       ((eq? void res) void)
       (else (set-cdr! kv res) void)
      )
     )
    )
   )
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
