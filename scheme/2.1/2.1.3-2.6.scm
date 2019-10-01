(define (log . args) (for-each display args) (newline))

(define (identity x) x)

(define (identity? f)
 (define (β) #t)

 (with-exception-handler
  (lambda (e) #f)
  (lambda () (eq? β (f β)))
 )
)

; Alonso Church's Zero is a tranformation of a function
; that takes any function and returns identity function.
(define church-zero
 (lambda (f) identity)
)

(define (church-zero? any)
 (define (β) #t)

 (with-exception-handler
  (lambda (e) #f)
  (lambda () (identity? (any β)))
 )
)

; Church's One is a tranformation of a function that takes any
; function and returns it, i.e., it's an identity tranformation,
; but warning: not an identity function!
(define church-one
 (lambda (f)
  (lambda (x) (f x))
 )
)

; Unlike pure identity function, identity transformation
; of a function returns not that function instance, but
; else function instance having the same results!
(define (church-one? any)
 (define (identity-tr? tr)
  (define (α x) 1)
  (define (β x) 2)
  (define (γ x) 3)
  (define (δ x) 4)

  (define (∂ x)
   (cond
    ((eq? x α) β)
    ((eq? x β) γ)
    (else δ)
   )
  )

  (and
   (not (eq? ∂ (tr ∂)))
   (eq? β ((tr ∂) α))
  )
 )

 (with-exception-handler
  (lambda (e) #f)
  (lambda () (identity-tr? any))
 )
)

; Church's Increment of Church's Number. A Number is a
; transformation of a function of single argument.
; It composes the function with the num-composition.
(define (church-inc church-num)
 (lambda (f)
  (lambda (x)
   (f
    ((church-num f)
     x
    )
   )
  )
 )
)


(log (church-one? church-zero) " 1 ?= 0")
(log (church-one? church-one) " 1 ?= 1" )
(log (church-one? (church-inc church-zero)) " 1 ?= (inc 0) ?= 1")
(log (church-one? (church-inc church-one)) " 1 ?= (inc 1)")

; Church's Two is 2-composition of a function: (f∘f)(x).
(define church-two
 (lambda (f)
  (lambda (x) (f (f x)))
 )
)

(define (church-two? any)
 (define (secondary-tr? tr)
  (define (α x) 1)
  (define (β x) 2)
  (define (γ x) 3)
  (define (δ x) 4)
  (define (ε x) 5)

  (define (∂ x)
   (cond
    ((eq? x α) β)
    ((eq? x β) γ)
    ((eq? x γ) δ)
    (else ε)
   )
  )

  (and
   (not (eq? ∂ (tr ∂)))
   (eq? γ ((tr ∂) α))
  )
 )

 (with-exception-handler
  (lambda (e) #f)
  (lambda () (secondary-tr? any))
 )
)

(log (church-two? church-zero) " 2 ?= 0" )
(log (church-two? church-one) " 2 ?= 1" )
(log (church-two? church-two) " 2 ?= 2" )
(log (church-two? (church-inc church-zero)) " 2 ?= (inc 0)")
(log (church-two? (church-inc church-one)) " 2 ?= (inc 1)")
(log (church-two? (church-inc church-two)) " 2 ?= (inc 2)" )
(log (church-two? (church-inc (church-inc church-zero))) " 2 ?= (inc (inc 0))")
