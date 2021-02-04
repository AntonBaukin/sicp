;
; Pattern matching implementation core.
; Depends on «defs.scm» and «utilities.scm».
;

; Takes untagged pattern and assertion, and recursively
; matches the assertion against the pattern by returning
; a frame, or void — if the matching have failed.
(define (pattern-match pattern assertion frame)
 (cond
  ; 1) May be due to 4) nested call:
  ((eq? void frame) void)

  ; 2) As assertion has no variables, this branch means
  ; the pattern literally matches it:
  ((equal? pattern assertion) frame)

  ; 3) Pattern has reduced to a variable pair: (? . some):
  ((variable? pattern)
   (extend-if-consistent pattern assertion frame)
  )

  ; 4) Do match recursively:
  ((and (pair? pattern) (pair? assertion))
   (pattern-match  ;<— cdrs
    (cdr pattern)
    (cdr assertion)
    (pattern-match ;<— cars
     (car pattern)
     (car assertion)
     frame
    )
   )
  )

  ; 5) Have failed to match:
  (else void)
 )
)

(define (extend-if-consistent var assertion frame)
 (define var-name (variable-name var))
 (define binding (frame-get frame var-name))

 (if (null? binding)
  (frame-bind frame var-name assertion)
  (pattern-match (binding-value binding) assertion frame)
 )
)

(define (unify-match a b frame)
 (cond
  ; 1) May be due to 5) nested call:
  ((eq? void frame) void)

  ; 2) The same expressions:
  ((equal? a b) frame)

  ; 3) Left expression is a variable:
  ((variable? a)
   (extend-if-possible a b frame)
  )

  ; 4) Right expression is a variable:
  ((variable? b)
   (extend-if-possible b a frame)
  )

  ; 4) Do match recursively:
  ((and (pair? a) (pair? b))
   (unify-match (cdr a) (cdr b)
    (unify-match (car a) (car b) frame)
   )
  )

  ; 5) Have failed to match:
  (else void)
 )
)

(define (unify-match-resolved a b frame)
 (define f (unify-match a b frame))

 (if (eq? void f) void
  (make-frame (resolve-all-vars (frame-bindings f)))
 )
)

(define (extend-if-possible var value frame)
 (define var-name (variable-name var))
 (define binding (frame-get frame var-name))

 (cond
  ((not (null? binding))
   (unify-match (binding-value binding) value frame)
  )

  ((variable? value)
   (let ((b (frame-get frame (variable-name value))))
    (if (null? b)
     (frame-bind frame var-name value)
     (unify-match var (binding-value b) frame)
    )
   )
  )

  ((exp-depends-on? value var frame)
   void
  )

  (else (frame-bind frame var-name value))
 )
)
