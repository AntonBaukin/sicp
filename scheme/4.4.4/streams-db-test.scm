(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "stream.scm")
(include "interfaces.scm")
(include "streams-db.scm")

(define db (make-streams-db))
(define db-get (streams-db-get db))
(define db-all (streams-db-all db))
(define db-add (streams-db-add db))

(define (db-items key)
 (stream->list (db-get key))
)

(define (db-all-items)
 (stream->list (db-all))
)

; Add first item.
(db-add 'A 1)
(assert-equal? '(1) (db-items 'A))

; Add more items by the same key.
(db-add 'A 2)
(assert-equal? '(2 1) (db-items 'A))
(db-add 'A 3)
(assert-equal? '(3 2 1) (db-items 'A))

; Add items by second key.
(db-add 'B 'a)
(assert-equal? '(a) (db-items 'B))
(db-add 'B 'b)
(assert-equal? '(b a) (db-items 'B))
(db-add 'B 'c)
(assert-equal? '(c b a) (db-items 'B))

; Test all items.
(assert-equal? '(3 2 1 c b a) (db-all-items))

; Add items by third key, and test all.
(db-add 'C '(a 1)) (db-add 'C '(b 2)) (db-add 'C '(c 3))
(assert-equal? '(3 2 1 c b a (c 3) (b 2) (a 1)) (db-all-items))
