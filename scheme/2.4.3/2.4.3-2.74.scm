(include "apply-generic.scm")

(define (log . args) (for-each display args) (newline))

; We take relatively simple routines of the Department A.
; We set three generic functions: 1) get the record by id;
; 2) get the salary as float point number; 3) search for
; the record by the required last name and optional first.
;
; Unified record has the following format:
; 0) tag of the department: symbols 'A or 'B;
; 1) id in the department's database;
; 2) "Last name, First name";
; 3) sex: 'Male or 'Female;
; 4) salary as a number;
; 5) contact phone string.
(define (install-db-a)
 (include "2.4.3-2.74-a.scm")

 ; Takes record in A-format and returns unified.
 (define (make-unified r)
  (if (null? r) '() (list
   'A
   (list-ref r 0)
   (string-append (get-last-name r) ", " (get-first-name r))
   (if (equal? "M" (get-sex r)) 'Male 'Female)
   (string->number (get-salary r))
   (get-phone r)
  ))
 )

 (define (get-unified-record key)
  (make-unified (get-record key))
 )

 (define (get-unified-salary key)
  (let ((r (get-record key)))
   (if (null? r) '() (string->number (get-salary r)))
  )
 )

 (define (unified-find-by-name last first)
  (make-unified (find-by-name last first))
 )

 ; Register unified functions for generic apply.
 (apply-generic-put-all
  'get-record   '(A)   get-unified-record
  'get-salary   '(A)   get-unified-salary
  'find-by-name '(A A) unified-find-by-name
 )
)

; Department B has completely else data structure: three
; normalized tables and the last name search index — all
; are represented as trees. To create a record it joins
; all three tables. See (install-db-a) for unified notes.
(define (install-db-b)
 (include "2.4.3-2.74-b.scm")

 ; Unified record differs only in the order of the fields.
 (define rec-projection '(0 1 2 4 3))

 (define (project r inds res)
  (if (null? inds) res
   (project r (cdr inds) (cons (list-ref r (car inds)) res))
  )
 )

 ; Takes record in B-format and returns unified.
 (define (make-unified r)
  (if (null? r) '() (reverse (project r rec-projection '(B))))
 )

 (define (get-unified-record id)
  (make-unified (get-record id))
 )

 (define (unified-find-by-name last first)
  (let ((id (find-by-name last first)))
   (if (null? id) '() (get-unified-record id))
  )
 )

 ; Register unified functions for generic apply.
 (apply-generic-put-all
  'get-record   '(B)   get-unified-record
  'get-salary   '(B)   get-salary
  'find-by-name '(B B) unified-find-by-name
 )
)

(define (get-record department id)
 (apply-generic
  'get-record
  (apply-generic-tag-set department id)
 )
)

(define (get-salary department id)
 (apply-generic
  'get-salary
  (apply-generic-tag-set department id)
 )
)

; Searches by the required last name and optional first one.
(define (find-by-name department last first)
 (apply-generic
  'find-by-name
  (apply-generic-tag-set department last)
  (apply-generic-tag-set department first)
 )
)

; Converts record in unified format to a string.
(define (unified-record->string r)
 (define (id->string id)
  (if (number? id) (number->string id) id)
 )

 (string-append
  "Dep: " (symbol->string (list-ref r 0)) "; "
  "Id: " (id->string (list-ref r 1)) "; "
  "Full Name: \"" (list-ref r 2) "\"; "
  "Sex: " (symbol->string (list-ref r 3)) "; "
  "Salary: " (number->string (list-ref r 4)) "; "
  "Phone: " (list-ref r 5)
 )
)

; Install generic database routines.
(install-db-a)
(install-db-b)

(log "—— Search result in Department A by key = \"080\": \n"
 (unified-record->string (get-record 'A "080")) "\n"
)

(log "—— Salary of person in Department A by key = \"122\": \n"
 (get-salary 'A "122") "\n"
)

(log "—— Person in Department A by Last Name = \"Laurentino\": \n"
 (unified-record->string (find-by-name 'A "Laurentino" '())) "\n"
)

(log "—— Person in Department A by Last Name = \"Laurentino\", "
 "First Name = \"Alice\": \n"
 (unified-record->string (find-by-name 'A "Laurentino" "Alice")) "\n"
)

(log "—— Search result in Department B by id = 5: \n"
 (unified-record->string (get-record 'B 5)) "\n"
)

(log "—— Salary of person in Department B by id = 6: \n"
 (get-salary 'B 6) "\n"
)

(log "—— Person in Department B by Last Name = \"Bunten\": \n"
 (unified-record->string (find-by-name 'B "Bunten" '())) "\n"
)

(log "—— Person in Department A by Last Name = \"Bunten\", "
 "First Name = \"Letti\": \n"
 (unified-record->string (find-by-name 'B "Bunten" "Letti")) "\n"
)
