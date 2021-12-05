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
 (define b (frame-get frame var-name))

 (cond
  ((null? b)
   (frame-bind frame var-name assertion)
  )

  ((parent-reference? var-name (binding-value b))
   (resolve-parent-variable frame var-name assertion)
  )

  (else
   (pattern-match (binding-value b) assertion frame)
  )
 )

 (if (null? b)
  (frame-bind frame var-name assertion)
  (pattern-match (binding-value b) assertion frame)
 )
)

(define (unify-match pattern rule frame)
 (cond
  ; 1) May be due to 5) nested call:
  ((eq? void frame) void)

  ; 2) The same symbols:
  ((eq? pattern rule) frame)

  ; 3) Pattern expression is a variable:
  ((variable? pattern)
   (extend-rule pattern rule frame)
  )

  ; 4) Rule expression is a variable:
  ((variable? rule)
   (extend-pattern pattern rule frame)
  )

  ; 5) Do match recursively:
  ((and (pair? pattern) (pair? rule))
   (unify-match (cdr pattern) (cdr rule)
    (unify-match (car pattern) (car rule) frame)
   )
  )

  ; 6) Have failed to match:
  (else void)
 )
)

(define (unify-match-resolved a b frame)
 (define f (unify-match a b frame))

 (if (eq? void f) void
  (make-frame (resolve-all-vars (frame-bindings f)))
 )
)

(define (parent-reference? var-name some)
 (and
  (variable? some)
  (eq? var-name (variable-name some))
 )
)

; Creates recursive reference on itself as a special flag.
(define (make-parent-reference var-name frame)
 (frame-bind frame var-name (cons '? var-name))
)

(define (extend-rule-frame-lookup var-name frame)
 (define binding (frame-get frame var-name))

 (log "LOOKUP " var-name " << " frame " ==>> " binding)
 (if (parent-reference? var-name binding)
  (extend-rule-frame-lookup var-name (frame-parent frame))
  binding
 )
)

(define (resolve-parent-variable frame var-name value)
 (log "RESOLVE " var-name " = " value " >> " (frame-bind frame var-name value))
 (frame-bind frame var-name value)
)

(define (extend-rule-couple-variables pattern-var rule-var frame)
 (define coupled-frame (frame-bind frame (variable-name rule-var) pattern-var))

 (if use-unique-frames coupled-frame
  (make-parent-reference (variable-name pattern-var) coupled-frame)
 )
)

(define (extend-rule pattern-var rule frame)
 (define binding (extend-rule-frame-lookup (variable-name pattern-var) frame))

 (log "EXT RULE> pattern-var = " pattern-var " >> rule = " rule " <<< " frame)

 (cond
  ((not (null? binding))
   (unify-match (binding-value binding) rule frame)
  )

  ((variable? rule)
   (let ((b (frame-get frame (variable-name rule))))
    (if (null? b)
     (extend-rule-couple-variables pattern-var rule frame)
     (begin
      ; (log "UMATCH pattern-var = " pattern-var " value = " (binding-value b))
      (unify-match pattern-var (binding-value b) frame)
     )
    )
   )
  )

  ((exp-depends-on? pattern-var rule frame)
   void
  )

  (else (frame-bind frame (variable-name pattern-var) rule))
 )
)

(define (extend-pattern pattern rule-var frame)
 (define var-name (variable-name rule-var))
 (define binding (frame-get frame var-name))

 (log "EXT PATTERN> pattern = " pattern " >> rule-var = " rule-var " <<< " frame)

 (cond
  ((not (null? binding))
   (unify-match pattern (binding-value binding) frame)
  )

  ((variable? pattern)
   (let ((b (frame-get frame (variable-name pattern))))
    (if (null? b)
     (frame-bind frame var-name pattern)
     (begin
      ; (log "UMATCH " (binding-value b) " rule-var = " rule-var)
      (unify-match (binding-value b) rule-var frame)
     )
    )
   )
  )

  ((exp-depends-on? pattern rule-var frame)
   void
  )

  (else (frame-bind frame var-name pattern))
 )
)
