(include "../2.3.3/tree.scm")

; In Department B employees database is split across
; several trees: names, salary, contacts joined by
; integer primary keys.
(define list-personnel-names '(
  (1 Male "Ben" "Ost")
  (2 Male "Ignaz" "Sealy")
  (3 Female "Caitlin" "Bunten")
  (4 Female "Cesya" "Kennington")
  (5 Male "Sawyere" "Labat")
  (6 Female "Letti" "Bunten")
  (7 Female "Brittaney" "Ruske")
  (8 Male "Axel" "O'Mahoney")
))

(define list-personnel-contacts '(
  (1 "+62-671-454-6369")
  (2 "+7-622-408-9896")
  (3 "+62-777-256-1970")
  (4 "+7-611-960-4872")
  (5 "+62-502-577-4951")
  (6 "+357-461-497-4665")
  (7 "+86-153-377-9901")
  (8 "+216-898-217-5853")
))

; Salary is an integer number.
(define list-personnel-salary '(
  (1 1200)
  (2 2150)
  (3 2000)
  (4 1000)
  (5 4000)
  (6 3900)
  (7 2250)
  (8 980)
))

; Compares two arbitrary record having the first item
; to be the integer primary key.
(define (rec-id<? a b) (< (car a) (car b)))

; A tree [type] of the keyed records.
(define DatabaseTable (make-tree rec-id<?))
(define db-tree<-list (tree-op<-list DatabaseTable))

; Here we create database trees from the convenient lists.
(define db-personnel-names (db-tree<-list list-personnel-names))
(define db-personnel-contacts (db-tree<-list list-personnel-contacts))
(define db-personnel-salary (db-tree<-list list-personnel-salary))

; Returns record of the given tree by the primary key.
(define (db-tree-get tree id)
 (define db-tree-search (tree-op-search DatabaseTable))
 ((tree-op-search DatabaseTable) tree (list id))
)

; Returns salary integer number by the primary key.
; Note the difference from the A database where salary
; is a fixed point number given as a string from the
; input record, not primary key!
(define (get-salary id)
 (let ((r (db-tree-get db-personnel-salary id)))
  (if (null? r) '() (cadr r))
 )
)

; Note the difference from the A database record!
; Combines data from each database into record:
; (id full-name sex phone salary).
(define (get-record id)
 (let* (
   (name (db-tree-get db-personnel-names id))
   (contacts (db-tree-get db-personnel-contacts id))
   (salary (get-salary id))
  )

  (if (null? name) '()
   (list
    (car name) ;—> id
    (string-append (cadddr name) ", " (caddr name))
    (cadr name)
    (cadr contacts)
    salary
   )
  )
 )
)

(define (rec-name-index<? a b) (string-ci<? (car a) (car b)))

; We additionaly create index that maps a name to the list
; of primary keys (the tail of the record).
(define NameIndex (make-tree rec-name-index<?))

; Takes a list of records and returns balanced tree
; of nodes (name id-x id-y ...).
(define (build-name-index records rec->name)
 (define (rec->item r)
  (list (rec->name r) (car r))
 )

 ; Appends id to the index item.
 (define (add-id res r)
  (cons
   (append (car res) (list (cadr r)))
   (cdr res)
  )
 )

 ; Joins the sorted records having the same name.
 (define (next res tail)
  (if (null? tail) res
   (let (
     (a (if (null? res) '() (caar res)))
     (b (caar tail)) ;—> the names to compare
    )

    (if (equal? a b)
     (next (add-id res (car tail)) (cdr tail))
     (next (cons (car tail) res) (cdr tail))
    )
   )
  )
 )

 (define build-tree (tree-op<-list NameIndex))

 (build-tree (reverse (next '()
  (quick-sort rec-name-index<? (map rec->item records))
 )))
)

; Index mapping the last names to the ids.
(define ix-personnel-names (build-name-index
 list-personnel-names
 (lambda (r) (list-ref r 3))
))

; For the given name index returns the list of ids.
(define (ix-find-by-name ix-tree name)
 (define search (tree-op-search NameIndex))
 (let ((res (search ix-tree (list name))))
  (if (null? res) '() (cdr res))
 )
)

; Searches by the required last name and optional first one.
; Returns the first id (not record, check A-version!), or '().
(define (find-by-name last first)
 ; Returns first name by the given id.
 (define (name-by-id id)
  (let ((name (db-tree-get db-personnel-names id)))
   (if (null? name) '() (caddr name))
  )
 )

 ; Searches for the match of the first name.
 (define (search ids)
  (if (null? ids) '()
   (let ((name (name-by-id (car ids))))
    (if (equal? name first) (car ids) (search (cdr ids)))
   )
  )
 )

 (let ((ids (ix-find-by-name ix-personnel-names last)))
  (cond
   ((null? ids) '())
   ((null? first) (car ids))
   (else (search ids))
  )
 )
)
