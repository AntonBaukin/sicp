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

; In original SICP implementation, matching rules and patterns
; is symmetrical, but task 79 forces us to treat them differently.
; Check extend-rule() and extend-pattern().
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

(define (frame-bind-coupled pattern-var rule-var frame)
 (if use-unique-frames
  (frame-bind frame (variable-name rule-var) pattern-var)
  (frame-bind frame (variable-name rule-var) pattern-var (- (frame-level frame) 1))
 )
)

(define (frame-resolve-variable frame level var-name)
 (define b (frame-get-at frame level var-name))

 (cond
  ((null? b) '())
  ((null? (binding-ext b))
   (binding-value b)
  )
  (else ; This variable also refers upper level:
   (frame-resolve-variable frame (car (binding-ext b)) var-name)
  )
 )
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
 (unify-match (binding-value pattern-binding) rule frame)
)

(define (unify-match-rule-binding pattern rule-binding frame)
 (unify-match pattern (binding-value rule-binding)
  (frame-bind-up-deps frame rule-binding pattern)
 )
)

; See extend-rule(). With nested frames each binding may have special
; extension: additional number being the up stack level. Value -1
; of this level has special meaning: the value of the binding
; of reference to the closest variable up the stack.
;
; Continue reading in frame-resolve-backward-links()...
;
(define (frame-bind-backward-links frame upper-var-name value)
 (frame-set-parent frame
  ; Mark backward links with -1 level:
  (frame-bind (frame-parent frame) upper-var-name value -1)
 )
)

; The following sample is for append implementation from task «4.4.1-61.scm»,
; and it is placed in the test of task 79, in file «4.4.4-79.scm»:
; (append (a) (b) ?z) => (append (a) (b) (a b)).
;
; When we deduce a variable, we move it's value up the stack of the frames.
; This sample has two frames: initial with level 0, and nested with level 1.
;
; Frame item (z (b) -1) has special level -1. It means that we need to move
; the deduced value (b) up the stack to level 0, where the same variable «z»
; has the value of ((? . u) ? . z). Note that it also refers itself, but
; from the upper level that does not exist. So, (b) is assigned to
; ((? . u) ? . ()) === ((? . u)) => u = b.
;
; (frame ((z (b) -1) (v ()) (u a) (y (b))) 1 (frame ((z ((? . u) ? . z) -1)) 0 ()))
;
(define (frame-resolve-backward-links frame)
 (define source (frame-bindings frame))

 (define (substitute v)
  (cond
   ((variable? v)
    (let ((x (find-binding source (variable-name v))))
     (if (null? x) v
      ; Possible cause of infinite recursion in case of cyclic references:
      (substitute (binding-value x))
     )
    )
   )

   ((pair? v)
    (cons (substitute (car v)) (substitute (cdr v)))
   )

   (else v)
  )
 )

 (define (resolve-binding b)
  (define ext (binding-ext b))

  ; { not a backward link? }
  (if (or (null? ext) (not (eq? -1 (car ext)))) b
   (make-binding
    (binding-name b)
    (substitute (binding-value b))
    ; the extension -1 is dropped
   )
  )
 )

 (define (resolve-bindings bindings result)
  (if (null? bindings) result
   (resolve-bindings
    (cdr bindings)
    (cons (resolve-binding (car bindings)) result)
   )
  )
 )

 (extend-frame (frame-parent frame)
  (resolve-bindings (frame-bindings (frame-parent frame)) '())
 )
)

; When extending a rule with pattern being a variable, we first try
; to take the value of this variable: if it exists, we continue
; the matching recursively. Else, we define this variable being
; the reference to yet unresolved pattern one.
;
; For standard implementation of SICP with unique names, we just
; save the variable object. For task 79 with nested frames we
; have to save special upward link object.
;
(define (extend-rule pattern-var rule frame)
 (define binding (frame-var-lookup pattern-var frame))

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

  (else
   (if use-unique-frames
    (frame-bind frame (variable-name pattern-var) rule)
    ; Pattern variable is defined in the parent frame of current
    ; rule processing as pattern comes from upper level:
    (frame-bind-backward-links frame (variable-name pattern-var) rule)
   )
  )
 )
)

; Extending pattern is easier than extending a rule as we just
; continue matcjing recursovely, if the variable exists, or,
; define a new binding with the value being the pattern,
; or the value of that pattern-a-variable.
(define (extend-pattern pattern rule-var frame)
 (define binding (frame-get frame (variable-name rule-var)))

 (cond
  ((not (null? binding))
   (unify-match-rule-binding pattern binding frame)
  )

  ((variable? pattern)
   (let ((b (frame-get frame (variable-name pattern))))
    (if (null? b)
     (frame-bind frame (variable-name rule-var) pattern)
     (unify-match-pattern-binding b rule-var frame)
    )
   )
  )

  ((exp-depends-on? pattern rule-var frame)
   void
  )

  (else (frame-bind frame (variable-name rule-var) pattern))
 )
)
