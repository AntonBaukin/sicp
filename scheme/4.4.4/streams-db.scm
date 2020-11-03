;
; Creates data structure required for the assertions
; and the rules database defined in SICP §4.4.4.5.
;
(include "../3.3.2/iterate.scm")
(include "../3.3.3/tree-red-black.scm")
(include "../3.3.3/table-tree.scm")

;
; The following declarations define the interface of
; Streams Database. For the given instance they return
; bound method of accessing the database.
;

; Streams getter.
; For the given key returns a stream of records.
; Returns empty stream when the key is not found.
; Arguments: (key-symbol).
(define (streams-db-get streams-db)
 (list-ref streams-db 0)
)

; Alls streams getter.
; Returns a stream that combines all the streams stored.
; Arguments: ().
(define (streams-db-all streams-db)
 (list-ref streams-db 1)
)

; Adds value to the database by the given key.
; Arguments: (key item).
(define (streams-db-add streams-db)
 (list-ref streams-db 2)
)

; Previously we used symbols as a keys.
; Here we replace it with strings as this is faster.
(define StringKeysTable (make-table-tree string-ci<?))

(define (make-streams-db)
 (define table ((table-op-make StringKeysTable)))
 (define lookup (table-op-lookup StringKeysTable))
 (define iterator (table-op-iterator StringKeysTable))
 (define add (table-op-add StringKeysTable))

 (define (key->string key)
  (if (symbol? key)
   (symbol->string key)
   (error "Not a symbol key for streams database" key)
  )
 )

 (define (record->iterator record)
  (list-iterator (cdr record))
 )

 (define (record->stream record)
  (if (eq? void record)
   the-empty-stream
   (iterator->stream
    (record->iterator record)
   )
  )
 )

 (define (get-stream key)
  (define record (lookup table (key->string key)))
  (record->stream record)
 )

 (define (get-all)
  (join-iterator->stream
   (iterator table)
   (lambda (recset)
    ; Hint: check what returns table iterator.
    (record->iterator (cadar recset))
   )
  )
 )

 (define (make-record key item)
  (cons key (list item))
 )

 (define (add-item key item)
  (define skey (key->string key))
  (define record (lookup table skey))

  (if (eq? void record)
   (add table (make-record key item) skey)
   (set-cdr! record (cons item (cdr record)))
  )
 )

 (list get-stream get-all add-item)
)
