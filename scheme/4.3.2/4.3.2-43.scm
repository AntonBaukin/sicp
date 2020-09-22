(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")

(eval-basic (debug on))

(eval-basic
 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (define (exclude items exs)
  (define (next result items)
   (cond
    ((null? items) (reverse result))
    ((member (car items) exs)
     (next result (cdr items))
    )
    (else
     (next (cons (car items) result) (cdr items))
    )
   )
  )

  (next '() items)
 )

 (define (make-person name)
  (define relative '())
  (define yacht '())

  (lambda (m . args)
   (cond
    ((eq? m 'name) name)
    ((eq? m 'get-relative) relative)
    ((eq? m 'set-relative) (set! relative (car args)))
    ((eq? m 'get-yacht) yacht)
    ((eq? m 'set-yacht) (set! yacht (car args)))
   )
  )
 )

 (define (relative person . args)
  (if (= 0 (length args))
   (person 'get-relative)
   (person 'set-relative (car args))
  )
 )

 (define daughter relative)
 (define father relative)

 (define (yacht person . args)
  (if (= 0 (length args))
   (person 'get-yacht)
   (person 'set-yacht (car args))
  )
 )

 (define Mary      (make-person 'Mary))
 (define Lorna     (make-person 'Lorna))
 (define Gabriella (make-person 'Gabriella))
 (define Melissa   (make-person 'Melissa))
 (define Rosalind  (make-person 'Rosalind))

 (define Daughters (list Mary Lorna Gabriella Melissa Rosalind))

 (define Moore     (make-person 'Moore))
 (define Downing   (make-person 'Downing))
 (define Hall      (make-person 'Hall))
 (define Bernacle  (make-person 'Bernacle))
 (define Parker    (make-person 'Parker))

 (define Fathers (list Moore Downing Hall Bernacle Parker))

 (define (exclude-fathers . exs)
  (amb-of (exclude Fathers exs))
 )

 (define (exclude-daughters . exs)
  (amb-of (exclude Daughters exs))
 )

 (define (father-names)
  (map
   (lambda (d) (cons (d 'name) ((father d) 'name)))
   Daughters
  )
 )

 (define (relate Father Daughter)
  (daughter Father Daughter)
  (father Daughter Father)
 )

 (define (father-correct? Daughter)
  (eq? Daughter (daughter (father Daughter)))
 )

 (define (daughter-correct? Father)
  (eq? Father (father (daughter Father)))
 )

 ; We also make yacht owning bidirectional.
 (define (own Owner Yacht)
  (yacht Yacht Owner)
  (yacht Owner Yacht)
 )

 ; Here we apply that a father is not naming his yacht by it's
 ; own daughter — this is explicitly said in the task.
 (define (solve-daughters with-Mary?)
  ; The follwing relations are pre-known:
  (if with-Mary? (relate Moore Mary))
  (relate Bernacle Melissa)

  ; The follwing yacht owners are known:
  (own Moore Lorna)
  (own Bernacle Gabriella)
  (own Downing Melissa)
  (own Hall Rosalind)

  ; It's clear, who owns «Mary»: it's dr. Parker...
  (own Parker Mary)

  ; Now we establish relations based on exclusions:
  ;
  (if (not with-Mary?)
   (father Mary
    (exclude-fathers
     (father Melissa)    ; Bernacle
     (yacht Mary)        ; Lorna
    )
   )
  )

  (father Gabriella
   (exclude-fathers
    (father Mary)        ; Moore | ?
    (father Melissa)     ; Bernacle
    (yacht Gabriella)    ; Bernacle
   )
  )

  (father Rosalind
   (exclude-fathers
    (father Mary)        ; Moore | ?
    (father Melissa)     ; Bernacle
    (father Gabriella)   ; ?
    (yacht Rosalind)     ; Hall
   )
  )

  (father Lorna
   (exclude-fathers
    (father Mary)        ; Moore | ?
    (father Melissa)     ; Bernacle
    (father Gabriella)   ; ?
    (father Rosalind)    ; ?
   )
  )

  (if (not with-Mary?)
   (daughter Moore
    (exclude-daughters
     (daughter Bernacle) ; Melissa
     (yacht Moore)
    )
   )
  )

  (daughter Downing
   (exclude-daughters
    (daughter Moore)     ; Mary | ?
    (daughter Bernacle)  ; Melissa
    (yacht Downing)      ; Melissa
   )
  )

  (daughter Hall
   (exclude-daughters
    (daughter Moore)     ; Mary | ?
    (daughter Bernacle)  ; Melissa
    (daughter Downing)   ; ?
    (yacht Hall)         ; Rosalind
   )
  )

  (daughter Parker
   (exclude-daughters
    (daughter Moore)     ; Mary | ?
    (daughter Bernacle)  ; Melissa
    (daughter Downing)   ; ?
    (daughter Hall)      ; ?
    (yacht Parker)       ; Mary
   )
  )

  ; The relations must be bidirectional:
  (require-all father-correct? Daughters)
  (require-all daughter-correct? Fathers)

  ; This is the cross-relation from the task:
  (require (eq? (yacht (father Gabriella)) (daughter Parker)))

  ((father Lorna) 'name)
  ; (father-names)
 )

 (global solve-daughters)
)

(log "——— Solve the problem of Daughters & Yachts ——— " "\n"
 (eval-amb-results (solve-daughters #t)) "\n"
)
;
; Answer is: (Downing)
;
; Here are the names of the fathers:
;   (Mary . Moore)
;   (Lorna . Downing)
;   (Gabriella . Hall)
;   (Melissa . Bernacle)
;   (Rosalind . Parker)

(log "——— The same task with Mary' father unknown ——— " "\n"
 (eval-amb-results (solve-daughters #f)) "\n"
)
;
; Answer is: (Downing Parker)
;
; Here are the names of the fathers:
;   (Mary . Moore)
;   (Lorna . Downing)
;   (Gabriella . Hall)
;   (Melissa . Bernacle)
;   (Rosalind . Parker)
;
;   (Mary . Hall)
;   (Lorna . Parker)
;   (Gabriella . Moore)
;   (Melissa . Bernacle)
;   (Rosalind . Downing)
