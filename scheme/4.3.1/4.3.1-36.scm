(define (log . args) (for-each display args) (newline))

(include "eval-amb.scm")

(eval-basic (debug on))
;
; Here we produce infinite combinations of (i j)
; pairs having the sum ascending.
;
(eval-basic
 (define (iab a b)
   (require (<= a b))
   (amb a (iab (+ a 1) b))
  )

 ; Note that in this function the recursion never ends!
 ; It stops because it uses «amb» that breaks on the results limit.
 (define (ijs)
  (define (next i sum)
   (if (<= (* i 2) sum)
    (amb (cons i (- sum i)) (next (+ i 1) sum))
    (next 1 (+ sum 1))
   )
  )

  (next 1 2)
 )

 (define (maxij ij)
  (if (> (car ij) (cdr ij)) (car ij) (cdr ij))
 )

 (define (sumij ij)
  (+ (car ij) (cdr ij))
 )

 (define (pythagorean? ij k)
  (=
   (square k)
   (+
    (square (car ij))
    (square (cdr ij))
   )
  )
 )

 (define (pythagorean-triples)
  (define ij (ijs))

  ; We choose k in [max(i, j), i + j] as:
  ; i² + j² = k² < (i + j)², i² < k² and j² < k².
  (define k (iab (maxij ij) (sumij ij)))

  (require (pythagorean? ij k))
  (list (car ij) (cdr ij) k)
 )

 (global pythagorean-triples)
)

(log "First 10 Pythagorean triples: "
 (eval-amb-lim 10 (pythagorean-triples)
 )
)

; Compare these results with task «3.5.3-69.scm» — they match.
; We may also compare the speeds: amb evaluation is twice slower.
;
; :> (3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)
; (12 16 20) (7 24 25) (10 24 26) (15 20 25) (20 21 29)
;
