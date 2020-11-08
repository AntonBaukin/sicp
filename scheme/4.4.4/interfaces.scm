;
; The following declarations define the interfaces
; used internally in Queries Evaluator, QEval.
;
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

; Returns a stream of assertions matching the given
; pattern and the frame (the latter may be ignored).
; Resulting stream may include all the database.
;
; Arguments: (pattern frame).
(define (assertions-db-fetch assertions-db)
 (list-ref assertions-db 0)
)

; Adds assertion made via «make-assertion()» from
; an income statement.
;
; Arguments: (assertion).
(define (assertions-db-add assertions-db)
 (list-ref assertions-db 1)
)
