(define (log . args) (for-each display args) (newline))

(define (mobile-make left right)
 (cons left right)
)

(define (mobile-branch-make length structure)
 (cons length structure)
)

(define mobile-branch-empty
 (cons 0 0)
)

(define (mobile-branch-left mobile)
 (car mobile)
)

(define (mobile-branch-right mobile)
 (cdr mobile)
)

(define (mobile-branch-length mobile-branch)
 (car mobile-branch)
)

(define (mobile-branch-structure mobile-branch)
 (cdr mobile-branch)
)

(define (mobile-structure-sub-mobile? mobile-branch)
 (pair? (mobile-branch-structure mobile-branch))
)

(define (mobile-branch-empty? mobile-branch)
 (if (mobile-structure-sub-mobile? mobile-branch) #f
  (= 0 (mobile-branch-structure mobile-branch))
 )
)

(define (mobile-branch-str mobile-branch)
 (if (mobile-branch-empty? mobile-branch) "0"
  (string-append
   "("
   (number->string (mobile-branch-length mobile-branch))
   " * "
   (if (mobile-structure-sub-mobile? mobile-branch)
    (mobile-str (mobile-branch-structure mobile-branch))
    (number->string (mobile-branch-structure mobile-branch))
   )
   ")"
  )
 )
)

(define (mobile-str mobile)
 (string-append
  "["
  (mobile-branch-str (mobile-branch-left mobile))
  " — "
  (mobile-branch-str (mobile-branch-right mobile))
  "]"
 )
)

(define (mobile-branch-weight mobile-branch)
 (if (mobile-structure-sub-mobile? mobile-branch)
  (mobile-total-weight (mobile-branch-structure mobile-branch))
  (mobile-branch-structure mobile-branch)
 )
)

(define (mobile-total-weight mobile)
 (+
  (mobile-branch-weight (mobile-branch-left mobile))
  (mobile-branch-weight (mobile-branch-right mobile))
 )
)

(define (mobile-branch-momentum mobile-branch)
 (*
  (mobile-branch-length mobile-branch)
  (mobile-branch-weight mobile-branch)
 )
)

(define (mobile-balanced? mobile)
 (define (branch-balanced? mobile-branch)
  (if (not (mobile-structure-sub-mobile? mobile-branch)) #t
   (mobile-balanced? (mobile-branch-structure mobile-branch))
  )
 )

 (and
  (=
   (mobile-branch-momentum (mobile-branch-left mobile))
   (mobile-branch-momentum (mobile-branch-right mobile))
  )
  (branch-balanced? (mobile-branch-left mobile))
  (branch-balanced? (mobile-branch-right mobile))
 )
)

(define (test-mobile)
 (define O mobile-branch-empty)

 (define A
  (mobile-make
   (mobile-branch-make 1 2)
   (mobile-branch-make 2 1)
  )
 )

 (define B
  (mobile-make
   O
   (mobile-branch-make 1 1)
  )
 )

 (define C
  (mobile-make
   (mobile-branch-make 3 2)
   (mobile-branch-make 2 A)
  )
 )

 (log "mobile A = " (mobile-str A))
 (log "mobile B = " (mobile-str B))
 (log "mobile C = " (mobile-str C))
 (log "")
 (log "weight A = " (mobile-total-weight A))
 (log "weight B = " (mobile-total-weight B))
 (log "weight C = " (mobile-total-weight C))
 (log "weight O = " (mobile-total-weight (mobile-make O O)))
 (log "")
 (log "bal.ed A ? " (mobile-balanced? A))
 (log "bal.ed B ? " (mobile-balanced? B))
 (log "bal.ed C ? " (mobile-balanced? C))
)

(log "")
(log "——————— Cons-Definition of Mobile ———————")
(log "")
(test-mobile)

(log "")
(log "")
(log "——————— List-Definition of Mobile ———————")
(log "")


(define (mobile-make left right)
 (list left right)
)

(define (mobile-branch-make length structure)
 (list length structure)
)

(define mobile-branch-empty
 (list 0 0)
)

(define (mobile-branch-right mobile)
 (cadr mobile)
)

(define (mobile-branch-structure mobile-branch)
 (cadr mobile-branch)
)

(test-mobile)
