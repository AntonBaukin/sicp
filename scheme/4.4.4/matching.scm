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

; (define DEPTH 0)
; (define (protect-depth)
;  (if (> DEPTH 50)
;   (raise "Depth limit!")
;   (set! DEPTH (+ 1 DEPTH))
;  )
; )

(define (extend-if-consistent var assertion frame)
 (define var-name (variable-name var))
 (define b (frame-get frame var-name))

; (log "EXT?Co var = " var " ::= " assertion " << " frame)
; (protect-depth)

 (cond
  ((null? b)
   (frame-bind frame var-name assertion)
  )

  ; This variable is coupled with else (not resolved) variable
  ; up the stack, thus may be assigned without pattern matching:
  ((not (null? (binding-ext b)))
   (frame-bind-up-deps frame b assertion)
  )

  (else
   (pattern-match (binding-value b) assertion frame)
  )
 )
)

(define (unify-match pattern rule frame)
; (log "UMATCH " pattern " <|> " rule " << " frame)
; (protect-depth)

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

(define (frame-bind-coupled pattern-var rule-var frame)
; (log "BIND " (variable-name rule-var) " := " pattern-var " >> "
;  (if use-unique-frames
;   (frame-bind frame (variable-name rule-var) pattern-var)
;   (frame-bind frame (variable-name rule-var) pattern-var (- (frame-level frame) 1))
;  )
; )

 (if use-unique-frames
  (frame-bind frame (variable-name rule-var) pattern-var)
  (frame-bind frame (variable-name rule-var) pattern-var (- (frame-level frame) 1))
 )
)

(define (frame-resolve-variable frame level var-name)
 (define b (frame-get-at frame level var-name))
; (log "RESOLVE " var-name " @ " level " := " b " << " frame)
 (if (null? b) '() (binding-value b))
)

(define (frame-resolve-binding binding frame)
 (define level (car (binding-ext binding)))
 (define value (binding-value binding))

 (if (not (variable? value)) value
  (frame-resolve-variable frame level (variable-name value))
 )
)

(define (frame-var-lookup pattern-var frame)
 (define b (frame-get-up frame (variable-name pattern-var)))
; (log "LOOKUP " pattern-var " @ " frame " :=> " b)

 (cond
  ((null? b) '())
  ((null? (binding-ext b)) b)
  (else (frame-resolve-binding b frame))
 )
)

(define (frame-bind-up-deps frame binding value)
 (define var-name (binding-name binding))
 (define ref-var (binding-value binding))
 (define ext (binding-ext binding)) ;<— (level) or ()

 ; Frame level of this binding:
 (define level (if (null? ext) '() (car ext)))

 ; Always assign the value in the current frame:
 (define result (frame-bind frame var-name value))

 ; Frame ancestor at the level (if defined):
 (define anc (if (null? level) result (frame-ancestor result level)))

; (log "UP! binding = " binding " := " value " << " frame)
; (log "    level = " level " ref-var = " ref-var)
; (log "    anc = " anc)

 (cond
  ; {binding has no level reference}
  ((null? level) result)

  ; {not a variable reference}
  ((not (variable? ref-var)) result)

  ; {no target ancestor level — maybe an error!}
  ((or (null? anc) (eq? anc result)) result)

  (else
   (let ((b (frame-get anc (variable-name ref-var))))
    (if (null? b)
     (frame-set-ancestor result
      ; The same value is set up the variables trace:
      (frame-bind anc (variable-name ref-var) value)
     )
     (frame-set-ancestor result
      ; Recursively assign the value up the stack:
      (frame-bind-up-deps anc b value)
     )
    )
   )
  )
 )
)

(define (unify-match-pattern-binding pattern-binding rule frame)
; (log "UMpB! pattern-binding = " pattern-binding " rule = " rule " <<< " frame)
 (unify-match (binding-value pattern-binding) rule frame)
)

(define (unify-match-rule-binding pattern rule-binding frame)
; (log "UMrB! pattern = " pattern " rule-binding = " rule-binding
;   " <<< " (frame-bind-up-deps frame rule-binding pattern)
; )

 (unify-match pattern (binding-value rule-binding)
  (frame-bind-up-deps frame rule-binding pattern)
 )
)

(define (extend-rule pattern-var rule frame)
 (define binding (frame-var-lookup pattern-var frame))

; (log "EXT RULE> pattern-var = " pattern-var " >> rule = " rule " <<< " frame)

 (cond
  ((not (null? binding))
   (unify-match-pattern-binding binding rule frame)
  )

  ((variable? rule)
   (let ((b (frame-get frame (variable-name rule))))
    (if (null? b)
     (frame-bind-coupled pattern-var rule frame)
     (unify-match-rule-binding pattern-var b frame)
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

; (log "EXT PATTERN> pattern = " pattern " >> rule-var = " rule-var " <<< " frame)

 (cond
  ((not (null? binding))
   (unify-match-rule-binding pattern binding frame)
  )

  ((variable? pattern)
   (let ((b (frame-get frame (variable-name pattern))))
    (if (null? b)
     (frame-bind frame var-name pattern)
     (unify-match-pattern-binding b rule-var frame)
    )
   )
  )

  ((exp-depends-on? pattern rule-var frame)
   void
  )

  (else (frame-bind frame var-name pattern))
 )
)
