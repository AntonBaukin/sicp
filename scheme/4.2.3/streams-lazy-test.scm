(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "streams-lazy.scm")

(eval-basic (debug on))

; Lazy streams: check pairs.
(assert-true? (eval-basic (pair? (cons 1 2))))
(assert-false? (eval-basic (pair? '())))

; Lazy streams: car & cdr.
(assert-eq? 1 (eval-resolve (car (cons 1 2))))
(assert-eq? 2 (eval-resolve (cdr (cons 1 2))))

; Lazy streams: check lists.
(assert-true? (eval-basic (list? '())))
(assert-true? (eval-basic (pair? (list 1))))
(assert-true? (eval-basic (list? (list 1))))
(assert-true? (eval-basic (list? (list 1 2 3))))

; Lazy streams: list cXr.
(assert-eq? 1 (eval-resolve (car (list 1))))
(assert-eq? 1 (eval-resolve (car (list 1 2 3))))
(assert-eq? 2 (eval-resolve (cadr (list 1 2 3))))
(assert-eq? 3 (eval-resolve (caddr (list 1 2 3))))
(assert-eq? '() (eval-resolve (cdddr (list 1 2 3))))

; Lazy streams: length.
(assert-eq? 0 (eval-resolve (length '())))
(assert-eq? 1 (eval-resolve (length (list 1))))

; Lazy streams: list-ref.
(assert-eq? 1 (eval-resolve (list-ref (list 1) 0)))
(assert-eq? 2 (eval-resolve (list-ref (list 1 2) 1)))
(assert-eq? 3 (eval-resolve (list-ref (list 1 2 3) 2)))

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

; Lazy streams: several lists map.
(assert-true?
 (eval-basic
  (define (sum a b) (+ a b))

  (lists-equal?
   (list 5 5 5 5)
   (map sum (list 1 2 3 4) (list 4 3 2 1))
  )
 )
)
