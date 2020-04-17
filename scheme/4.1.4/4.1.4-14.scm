(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.4/4.1.4-14-primes.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")

; We have to use raw imports of dispatching evaluator
; as it always includes standard routines, namely,
; «map» — that Hugo wants to experiment with.
(include "../2.5.1/defined.scm")
(include "../4.1.1/eval-env-tree.scm")
(include "../4.1.1/eval-env.scm")
(include "../4.1.1/eval.scm")
(include "../4.1.2/eval-disp-includes.scm")
(include "../4.1.1/eval-basic-routine.scm")

; Here we exclude definitions of standard routines:
; We include it later when see, what's wrong with
; the Hugo's proposal on native map...
;(include "../4.1.1/eval-basic-std.scm")


; Enable debug mode:
(eval-basic (debug on))


; At the first glance, Hugo is almost right.
; The following tests show that primary «map»
; correctly works with mapper being a primary
; function itself:

(assert-equal? '(a b c)
 (eval-basic
  (map car '((a 1) (b 2) (c 3)))
 )
)

(assert-equal? '((a 1) (b 2) (c 3))
 (eval-basic
  (map list '(a b c) '(1 2 3))
 )
)

; But as we want to use our own functions as mappers,
; we can't use underlying «map» as it does not expert
; to receive a list of '(procedure ...) treated as
; a function only inside our evaluator...
(assert-error
 (lambda ()
  (eval-basic
   (map
    (lambda (a b c) (apply + (list a b c)))
    '(100 200 300) '(10 20 30) '(1 2 3)
   )
  )
 )

 (lambda (message args)
  (log "\n" "—— Expected error of Hugo's primary map: ——" "\n")
  (log "   Error message: " message)
  (log "   Argument: " (car args))
 )
)


; So, Eve creates «map» function inside the evaluator,
; where «apply» form works correcltly with not-primary
; functions and lambdas defined inside.
(eval-basic
 ; Note, that we have no tail recursion optimization,
 ; thus it has no match to underlying «map».
 (define (std-map-with with streams res)
  (if (null? streams)
   (reverse res)
   (std-map-with
    with
    (cdr streams)
    (cons (with streams) res)
   )
  )
 )

 (define (std-map-next mapper streams res)
  (if (null? (car streams))
   (reverse res)
   (std-map-next
    mapper
    (std-map-with cdar streams '())
    (cons
     (apply mapper (std-map-with caar streams '()))
     res
    )
   )
  )
 )

 (define (std-map mapper . streams)
  (std-map-next mapper streams '())
 )

 ; Special form «global» allows us to define variable
 ; directly in global environment. Each call for «eval»
 ; creates eval-private environment that inherits from
 ; the global one, and ordinary «define» registers
 ; there — but we want to use our «map» in the
 ; following calls of «eval-basic».
 (global map std-map)
)

(assert-equal? '(111 222 333)
 (eval-basic
  (map
   (lambda (a b c) (apply + (list a b c)))
   '(100 200 300) '(10 20 30) '(1 2 3)
  )
 )
)

(log "\n" "Eve's «map» successfully tested!" "\n")
