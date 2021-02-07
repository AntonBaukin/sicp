(include "../4.4.4/qeval-test-base.scm")

(add-rule (same ?x ?x))

(add-rule (lives-near ?person-a ?person-b)
 (and
  (address ?person-a (?town . ?address-a))
  (address ?person-b (?town . ?address-b))
  (not (same ?person-a ?person-b))
 )
)

(log "  ————  " "Who lives near Alyssa" "  ————")
(test-and-log
 (lives-near ?person (Hacker Alyssa P))
; —————————————————————————————————————————————————————————
 (lives-near (Fect Cy D) (Hacker Alyssa P))
)


(log "\n" "  ————  " "Neighbours with pairs" "  ————")
(test-and-log
 (lives-near ?person-a ?person-b)
; —————————————————————————————————————————————————————————
 (lives-near (Aull DeWitt) (Doom Hugo))
 (lives-near (Aull DeWitt) (Bitdiddle Ben))
 (lives-near (Doom Hugo) (Aull DeWitt))
 (lives-near (Doom Hugo) (Bitdiddle Ben))
 (lives-near (Hacker Alyssa P) (Fect Cy D))
 (lives-near (Fect Cy D) (Hacker Alyssa P))
 (lives-near (Bitdiddle Ben) (Aull DeWitt))
 (lives-near (Bitdiddle Ben) (Doom Hugo))
)


(define (names-<? a b)
 (cond
  ((and (null? a) (null? b)) #f)
  ((and (pair? a) (pair? b))
   (let ((x (names-<? (car a) (car b))))
    (if x #t
     (let ((y (names-<? (car b) (car a))))
      (if y #f (names-<? (cdr a) (cdr b)))
     )
    )
   )
  )
  ((pair? a) #f)
  ((pair? b) #t)
  (else
   (string-ci<?
    (symbol->string a)
    (symbol->string b)
   )
  )
 )
)

(add-rule (lives-near-distinct ?person-a ?person-b)
 (and
  (address ?person-a (?town . ?address-a))
  (address ?person-b (?town . ?address-b))
  (lisp-value names-<? (quote ?person-a) (quote ?person-b))
 )
)

(log "\n" "  ————  " "Neighbours distinct" "  ————")
(test-and-log
 (lives-near-distinct ?person-a ?person-b)
; —————————————————————————————————————————————————————————
 (lives-near-distinct (Aull DeWitt) (Doom Hugo))
 (lives-near-distinct (Aull DeWitt) (Bitdiddle Ben))
 (lives-near-distinct (Fect Cy D) (Hacker Alyssa P))
 (lives-near-distinct (Bitdiddle Ben) (Doom Hugo))
)
