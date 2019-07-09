(include "2.5.2-arithmetics.scm")
(include "2.5.2-tower.scm")

(define (log . args) (for-each display args) (newline))

; Hugo creates this trivial coercions...
(put-coercion '(number number) (lambda (v)
 (make-number v) ; Note that the value comes unwrapped
))

(put-coercion '(complex complex) (lambda (z)
 (num-tag-set 'complex z) ; Just wrap the complex back
))

; Now he adds exponent for real numbers
((apply-generic-scope-register numbers-scope)
 'exp '(number number)
 (lambda (x y) (num-tag-set 'number (expt x y)))
)

(define exp (curry num-call 'exp))


; It works for two reals as this generic call is directly
; registered in the table and no coercion takes place.
(log "2. ^ 4. = " (num->str
 (exp (make-number 2) (make-number 4))
))

; This leads to infinite recursion!
; (exp (make-complex-xy 2 0) (make-complex-xy 4 0))

; The best way to solve this problem is to check whether
; the types after the coercion are same: if so, repeated
; call to apply-generic — for the numbers scope it is
; num-call() — that leads to infinite recursion.
(define (num-call-apply-coerced op-symbol arg-symbols-list coerced)
 (let ((coerced-types (map num-tag-get coerced)))
  (if (equal? arg-symbols-list coerced-types)
   (error "Repeated coercion of types" coerced-types)
   (apply num-call (append (list op-symbol) coerced))
  )
 )
)

; Now, this call prints explicit error!
; (exp (make-complex-xy 2 0) (make-complex-xy 4 0))

; Save the original implementation.
(define try-coerce-2-impl try-coerce-2)

; Re-define the coerce-2. Now we explicitly check
; that the types are the same, but it's better to
; forbid to even register such a coercions!
(define (try-coerce-2 t1 t2 v1 v2)
 (if (eq? t1 t2) '()
  (try-coerce-2-impl t1 t2 v1 v2)
 )
)

; This direct match still works...
(exp (make-number 2) (make-number 4))

; And this call now reports «Can't coerce arguments».
; We could tell it more clear: there is no operation...
(log "The following error must be:")
(exp (make-complex-xy 2 0) (make-complex-xy 4 0))
