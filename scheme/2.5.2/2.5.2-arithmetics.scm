(include "../2.5.1/defined.scm")

; Takes the list of the arguments types and the unwrapped
; values and returns the list of the arguments coerced,
; or an empty list, if coercion is not possible.
;
; Here we implement the simplest verion working with two
; arguments only, as defined in §2.5.2. It's re-defined
; in the following exercises.
;
(define (try-coerce arg-symbols-list arguments)
 (if (> (length arguments) 2) '() (try-coerce-2
  (car arg-symbols-list) (cadr arg-symbols-list)
  (car arguments) (cadr arguments)
 ))
)

(define (try-coerce-2 t1 t2 v1 v2)
 (let ((t1->t2 (get-coercion (list t1 t2))))
  (if (procedure? t1->t2)
   (list (t1->t2 v1) (num-tag-set t2 v2))
   (let ((t2->t1 (get-coercion (list t2 t1))))
    (if (procedure? t1->t2)
     (list (num-tag-set t1 v1) (t2->t1 v2))
     (error "Can't coerce 2-arguments" t1 t2)
    )
   )
  )
 )
)


; This is fallback function for the numbers scope
; apply-generic. Instead of overwriting apply-generic
; directly, as it's in §2.5.2, we define this fallback
; strategy. It delegates coercion and recursively calls
; apply-generic as it's done in SICP book.
(define (num-call-fallback op-symbol arg-symbols-list arguments)
 (let ((coerced (try-coerce arg-symbols-list arguments)))
  ; (log "coerced " arg-symbols-list " to " coerced)
  (if (null? coerced)
   (error "Can't coerce arguments" arg-symbols-list)
   (apply num-call (append (list op-symbol) coerced))
  )
 )
)


; We include arithmetics here as we pre-define
; special version of num-call-fallback extension
; point of apply-generic for the numbers.
(include "../2.5.1/2.5.1-arithmetics.scm")


; Here we just add integer numbers, the same as plain numbers,
; but with additional check when making.
(define (install-integer-package scope)
 (install-number-package-base scope '(integer))
)

(define-value-if-not 'make-integer (lambda (value)
 (if (integer? value)
  (make-num 'num 'integer value)
  (error "Not an integer number value" value)
 )
))

; We alse re-define the number maker to always be float.
(define (make-number v)
 (make-num 'num 'number (* 1.0 v))
)


; Register integers package. Now we have four of them.
(define-value-if-not 'integer-package-init install-integer-package)
(install-arithmetic-package 'integer-package integer-package-init)


; Stores coercions in the apply-generic table of the numbers.
; For the format of a cource function, see standart coercions.
(define (put-coercion arg-symbols-list coerce-function)
 ((apply-generic-scope-register numbers-scope)
  'coerce arg-symbols-list coerce-function
 )
)

; Lookup for a coercion.
(define (get-coercion arg-symbols-list)
 ((apply-generic-scope-lookup numbers-scope)
  'coerce arg-symbols-list
 )
)
