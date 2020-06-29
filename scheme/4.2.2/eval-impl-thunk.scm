;
; This implementation is essentially the same as one we
; created in «4.1.6/eval-impl-forms.scm» for promises.
;

; In this implementation we tag with inner definition
; not visible from the outside.
;(define (THUNK) void)
;(define (RESOLVED) void)
(define THUNK 'THUNK)
(define RESOLVED 'RESOLVED)

; In SICP this function is named as «delay-it».
(define (make-thunk some env)
 (if (not (procedure? some))
  (error "Only analyzer executor may be thunked" some)
 )

 (list THUNK some env)
)

(define (thunk-them them env)
 (map (lambda (some) (make-thunk some env)) them)
)

(define (thunk? x)
 (and
  (list? x)
  (= 3 (length x))
  (eq? THUNK (car x))
 )
)

(define (resolved-thunk? x)
 (and
  (list? x)
  (= 3 (length x))
  (eq? THUNK (car x))

  ; We store this flag in the previous position of the expression.
  ; Memoized value is stored instead of the environment.
  (eq? RESOLVED (cadr x))
 )
)

(define (get-resolved-thunk-value thunk)
 (caddr thunk)
)

(define (invoke-thunk p)
 ((cadr p) (caddr p))
)

(define (resolve-thunk p)
 ; The value of thunk must be an analyzed executor.
 ;
 ; The big deal here is that, unlike in pure eval,
 ; in analyzing eval the result may a thunk, thus
 ; it must be resolved recursively!
 ;
 ; So, in our implementation thunks do form a tree
 ; of analyzed executor resolving.
 ;
 (define th (invoke-thunk p))
 (define result (resolve-value th))

 ; Memoize the value in the same record:
 (set-car! (cdr p) RESOLVED)
 (set-car! (cddr p) result)

 result
)

; In SICP this function is named as «force-it».
; Note, that in our version there is no «actual-value»
; function that evaluates before the resolve — as we
; use analyzer, see «eval-disp-apply-lazy».
(define (resolve-value some)
 (cond
  ((resolved-thunk? some)
   (get-resolved-thunk-value some)
  )

  ((thunk? some)
   (resolve-thunk some)
  )

  ; We expect this to be a direct value
  ; not wrapped into a thunk:
  (else some)
 )
)
