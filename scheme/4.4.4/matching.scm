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
  ;    the pattern literally matches it:
  ((equal? pattern assertion) frame)

  ; 3) Pattern has reduced to a variable pair: (? . some):
  ((variable? pattern)
   (extend-if-consistent (variable-name pattern) assertion frame)
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

  ; 5) have failed to match:
  (else void)
 )
)

(define (extend-if-consistent var-name assertion frame)
 (define binding (frame-get frame var-name))

 (if (null? binding)
  (frame-bind frame var-name assertion)
  (pattern-match (binding-value binding) assertion frame)
 )
)
