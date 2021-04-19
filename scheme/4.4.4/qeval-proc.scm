;
; Procedures of QEval: and, or, not, lisp-value.
; Depends on: «stream.scm», «defs.scm», and «utilities.scm».
;

(define (qproc-and conjuncts frame-stream)
 (if (null? conjuncts)
  frame-stream
  (qproc-and
   (cdr conjuncts)
   (qeval-disp
    (make-pattern (car conjuncts))
    frame-stream
   )
  )
 )
)

(define (qproc-or disjuncts frame-stream)
 (if (null? disjuncts)
  the-empty-stream
  (interleave-delayed
   (qeval-disp
    (make-pattern (car disjuncts))
    frame-stream
   )
   (delay
    (qproc-or (cdr disjuncts) frame-stream)
   )
  )
 )
)

(define (qproc-not operands frame-stream)
 (stream-flatmap
  (lambda (frame)
   (define single-frame (singleton-stream frame))
   (define negated-frames
    (qeval-disp
     (make-pattern (car operands))
     single-frame
    )
   )

   (if (stream-null? negated-frames)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (lisp-value-match? call frame)
 (eval (prepare-for-eval (instantiate call frame)))
)

(define (qproc-lisp-value call frame-stream)
 (stream-flatmap
  (lambda (frame)
   (if (lisp-value-match? call frame)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (qproc-always-true ignore frame-stream)
 frame-stream
)

; Allows to add variable to a frame in to variants:
;
; 1) (set ?x exp) where «exp» may be any plain value or
;    list composition with else variables.
;
; 2) (set ?x fname args...) where «fname» is a name of
;    global function taking some arguments, same as
;    «lisp-value» special rule takes.
;
; This form allows us to build new values that may not
; be deduced, such as in recursive samples of SICP,
; that cause infinite progression.
;
; Special assignment of form ?:x treats name «x» as global
; variable, and assigns that value excluding unique prefix.
; This allows to pass values up the «stack».
;
(define (qproc-set exp frame-stream)
 (define (ordinary? name)
  (not (eq? #\: (string-ref name 0)))
 )

 (define (assign-special frame name value)
  (frame-bind
   frame
   (string->symbol (substring name 1 (string-length name)))
   value
  )
 )

 (define (assign frame name value)
  (define u (decode-unique-var name))

  (if (or (null? u) (ordinary? (car u)))
   (frame-bind frame name value)
   (assign-special frame (car u) value)
  )
 )

 (define (set-value frame exp)
  (assign
   frame
   (variable-name (car exp))
   (cadr exp)
  )
 )

 (define (set-eval frame exp)
  (assign
   frame
   (variable-name (car exp))
   (eval (prepare-for-eval (instantiate (cdr exp) frame)))
  )
 )

 (cond
  ((and (= (length exp) 2) (variable? (car exp)))
   (stream-flatmap
    (lambda (frame)
     (singleton-stream (set-value frame exp))
    )
    frame-stream
   )
  )
  ((and (> (length exp) 2) (variable? (car exp)))
   (stream-flatmap
    (lambda (frame)
     (singleton-stream (set-eval frame exp))
    )
    frame-stream
   )
  )
  (else (error "Wrong «set» form" exp))
 )
)

; Simple «amb» form. The syntax is following:
; (amb ?x value) where value may be: a plain list,
; a variable, a list of variables, produce function.
;
; Produce function must be defined globally, and is
; given by the name symbol. Hence, the value may not
; be a plain symbol, or it's treated as a function.
; Function takes (value) and returns the next one,
; or empty list to stop. Initial value on the first
; call is also empty list.
;
; Form produces a stream of frames where «?x» takes
; each value from the list.
;
(define qproc-amb ;
 (
  (lambda () ;<— immediately invoked function
   (define (produce-streams frame-stream var producer-name)
    (define producer (eval producer-name))

    (stream-flatmap
     (lambda (frame)
      (produce-stream frame var producer)
     )
     frame-stream
    )
   )

   (define (produce-stream frame var producer)
    (define value '())

    (define (next-stream)
     (set! value (producer value))
     (if (null? value)
      the-empty-stream
      (cons-stream
       (frame-bind frame var value)
       (next-stream) ;<— delayed
      )
     )
    )

    (next-stream)
   )

   (define (amb-any var value frame)
    (cond
     ((variable? value)
      (let ((b (frame-get frame (variable-name value))))
       (if (null? b)
        (singleton-stream (frame-bind frame var value))
        (amb-any var (binding-value b) frame)
       )
      )
     )

     ((list? value)
      (stream-flatmap
       (lambda (item)
        (amb-any var item frame)
       )
       (list->stream value)
      )
     )

     ((pair? value)
      (amb-any var (list (car value) (cdr value)) frame)
     )

     (else
      (singleton-stream (frame-bind frame var value))
     )
    )
   )

   (lambda (exp frame-stream) ;<— resulting procedure
    (cond
     ((and (= (length exp) 2) (variable? (car exp)) (symbol? (cadr exp)))
      (produce-streams frame-stream (variable-name (car exp)) (cadr exp))
     )

     ((and (= (length exp) 2) (variable? (car exp)))
      (stream-flatmap
       (lambda (frame)
        (amb-any (variable-name (car exp)) (cadr exp) frame)
       )
       frame-stream
      )
     )

     (else (error "Wrong «amb» form" exp))
    )
   )
  )
 )
)

; This HOF wraps call to a procedure to allow it's redefinition
; in files included after this file.
(define (call-proc proc-lambda)
 (lambda (exp frame-stream)
  (proc-lambda exp frame-stream)
 )
)

(define qeval-procs-std (list
 (list 'and
  (call-proc (lambda (e fs) (qproc-and e fs)))
 )

 (list 'or
  (call-proc (lambda (e fs) (qproc-or e fs)))
 )

 (list 'not
  (call-proc (lambda (e fs) (qproc-not e fs)))
 )

 (list 'lisp-value
  (call-proc (lambda (e fs) (qproc-lisp-value e fs)))
 )

 (list 'always-true qproc-always-true)
 (list 'set qproc-set)
 (list 'amb qproc-amb)
))
