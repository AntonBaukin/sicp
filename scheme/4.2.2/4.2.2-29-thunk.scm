;
; This implementation uses thunks without memoization.
; Compare it with «eval-impl-thunk.scm».
;

(define THUNK 'thunk)

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
 (resolve-value (invoke-thunk p))
)

; In SICP this function is named as «force-it».
; Note, that in our version there is no «actual-value»
; function that evaluates before the resolve — as we
; use analyzer, see «eval-disp-apply-lazy».
(define (resolve-value some)
 (if (thunk? some)
  (resolve-thunk some)
  ; We expect this to be a direct value
  ; not wrapped into a thunk:
  some
 )
)
