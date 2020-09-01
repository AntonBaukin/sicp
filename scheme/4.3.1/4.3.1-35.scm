(define (log . args) (for-each display args) (newline))

(include "eval-amb.scm")

(eval-basic (debug on))
;
; Compare this with task «3.5.3-69» that produces
; stream of triples with (i j) sum ascending.
; We will use this order in the next task...
;
(log "Pythagorean triples below 15: "
 (eval-amb-results
  (define (integer-between a b)
   (require (<= a b))
   (amb a (integer-between (+ a 1) b))
  )

  (define (pythagorean-triple-between a b)
   (let ((i (integer-between a b)))
    (let ((j (integer-between i b)))
     (let ((k (integer-between j b)))
      (require (= (+ (* i i) (* j j)) (* k k)))
      (list i j k)
     )
    )
   )
  )

  (pythagorean-triple-between 1 15)
 )
)

; It prints as expected:
; > ((3 4 5) (5 12 13) (6 8 10) (9 12 15))
; but it first tries each (i ...).
