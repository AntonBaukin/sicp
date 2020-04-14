;
; We define own implementation of environment frame.
; See «eval-env-tree.scm» for the interface required.
;

(define (log . args) (for-each display args) (newline))

; Frame object is altered, and we wrap it in a tagged pair.
(define (eval-env-frame-make)
 (cons 'env-frame '())
)

; In a frame based on the tree table we replace existing
; value. To follow the same requirement, we also search.
(define (eval-env-frame-add frame value name)
 (define (search tail)
  (if (null? tail) '()
   (if (eq? name (caar tail))
    (car tail) ;<— return the (key . value) pair to alter
    (search (cdr tail))
   )
  )
 )

 (define p (search (cdr frame)))

 (if (null? p)
  ; Alter the frame by adding new head to the list:
  (set-cdr! frame (cons (cons name value) (cdr frame)))
  ; Alter the pair found:
  (set-cdr! p value)
 )
)

(define (eval-env-frame-lookup frame name)
 (define (search tail)
  (if (null? tail) void
   (if (eq? name (caar tail))
    (cdar tail) ;<— return the value of the pair found
    (search (cdr tail))
   )
  )
 )

 (search (cdr frame))
)

(define (eval-env-frame-iterate frame iter)
 (define (next tail) ;<— tail is not null
  (define r (iter (caar tail) (cdar tail)))

  (if (not (eq? void r)) r
   (if (not (null? (cdr tail)))
    (next (cdr tail))
   )
  )
 )

 (if (null? (cdr frame)) void (next (cdr frame)))
)

;
; Here we include dispatching evaluator, all except
; the environment frame implementation — instead
; of red-black tree we use a list of pairs.
;
(include "../2.5.1/defined.scm")
(include "../4.1.1/eval-env.scm")
(include "../4.1.1/eval.scm")
(include "../4.1.2/eval-disp-includes.scm")
(include "../4.1.1/eval-basic-routine.scm")
(include "../4.1.1/eval-basic-std.scm")


; Now we run overall tests:
(include "../3.3.2/assert.scm")
(include "../4.1.1/eval-test-items.scm")
