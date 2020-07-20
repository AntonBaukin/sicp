(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "streams-lazy.scm")

(eval-basic (debug on))

; Lazy streams: check pairs.
(assert-true? (eval-basic (pair? (cons 1 2))))
(assert-false? (eval-basic (pair? '())))

; Lazy streams: car & cdr.
(assert-eq? 1 (eval-basic (car (cons 1 2))))
(assert-eq? 2 (eval-basic (cdr (cons 1 2))))

; Lazy streams: check lists.
(assert-true? (eval-basic (list? '())))
(assert-true? (eval-basic (pair? (list 1))))
(assert-true? (eval-basic (list? (list 1))))
(assert-true? (eval-basic (list? (list 1 2 3))))

; Lazy streams: list cXr.
(assert-eq? 1 (eval-basic (car (list 1))))
(assert-eq? 1 (eval-basic (car (list 1 2 3))))
(assert-eq? 2 (eval-basic (cadr (list 1 2 3))))
(assert-eq? 3 (eval-basic (caddr (list 1 2 3))))
(assert-eq? '() (eval-basic (cdddr (list 1 2 3))))

; Lazy streams: length.
(assert-eq? 0 (eval-basic (length '())))
(assert-eq? 1 (eval-basic (length (list 1))))

; Lazy streams: list-ref.
(assert-eq? 1 (eval-basic (list-ref (list 1) 0)))
(assert-eq? 2 (eval-basic (list-ref (list 1 2) 1)))
(assert-eq? 3 (eval-basic (list-ref (list 1 2 3) 2)))

; Lazy streams: lists equality.
(assert-true? (eval-basic (lists-equal? '() (list))))
(assert-true? (eval-basic (lists-equal? (list 1) (list 1))))
(assert-true? (eval-basic (lists-equal? (list 1 2) (list 1 2))))
(assert-false? (eval-basic (lists-equal? (list) (list 1))))
(assert-false? (eval-basic (lists-equal? (list 1 2) (list 1))))

; Lazy streams: lists map.
(assert-true?
 (eval-basic
  (lists-equal?
   (list 4 9 16 25)
   (map square (list 2 3 4 5))
  )
 )
)
