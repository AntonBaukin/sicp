; In Department A employees database is a list of
; records (id first-name last-name sex salary phone)
; each being a string: also, the salary and the key.
(define db-employees '(
  ("001" "Jacquelyn" "Lynch" "F" "2000.00" "+1-178-798-3065")
  ("010" "Yasmin" "Olson" "F" "1500.00" "+1-472-104-4902")
  ("015" "Kamron" "Beatty" "F" "1500.00" "+1-615-790-5895")
  ("080" "Stacie" "Laurentino" "F" "3000.00" "+1-615-790-5895")
  ("121" "Alice" "Laurentino" "F" "2250.00" "+233-878-498-8973")
  ("122" "Elisha" "Huxton" "M" "1650.00" "+53-551-703-4291")
  ("123" "Jerrold" "Moffet" "M" "2100.00" "+48-280-814-8458")
  ("124" "Marlow" "Hartless" "M" "3100.00" "+1-701-919-4184")
  ("125" "Tracey" "Copnall" "F"  "2900.00" "+351-103-248-3981")
))

; Iterates over the database records: if cursor returns #t,
; breakes returning the stop-record, else returns '().
(define (db-each cursor)
 (define (next tail)
  (if (null? tail) '()
   (if (eq? #t (cursor (car tail)))
    (car tail)
    (next (cdr tail))
   )
  )
 )

 (next db-employees)
)

; Returns record by the string key exact match.
(define (get-record key)
 (db-each (lambda (r) (equal? key (car r))))
)

(define (get-first-name r)
 (if (null? r) '() (list-ref r 1))
)

(define (get-last-name r)
 (if (null? r) '() (list-ref r 2))
)

(define (get-sex r)
 (if (null? r) '() (list-ref r 3))
)

; Returns the salary (string number) of the given record.
(define (get-salary r)
 (if (null? r) '() (list-ref r 4))
)

(define (get-phone r)
 (if (null? r) '() (list-ref r 5))
)

; Searches by the required last name and optional first one.
; Returns the first record matching, or '().
(define (find-by-name last first)
 (db-each (lambda (r)
  (and
   (equal? last (get-last-name r))
   (or
    (null? first)
    (equal? first (get-first-name r))
   )
  )
 ))
)
