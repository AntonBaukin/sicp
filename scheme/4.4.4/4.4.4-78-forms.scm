(eval-basic
 ; «And» form is structurally the same:
 (define (qeval-and conjuncts frame)
  (if (null? conjuncts)
   frame
   (qeval-and
    (cdr conjuncts)
    (qeval-disp
     (make-pattern (car conjuncts))
     frame
    )
   )
  )
 )

 (global qeval-and)

 (define (qeval-or disjuncts frame)
  (require (not (null? disjuncts)))

  (amb
   (qeval-disp
    (make-pattern (car disjuncts))
    frame
   )
   (qeval-or (cdr disjuncts) frame)
  )
 )

 (global qeval-or)

 (define (qeval-not operands frame)
  (define passed '())

  ; The implementation is tricky: we have to use two special
  ; forms that overcome standard behaviour of Amb evaluator.
  ;
  ; Form «amb-catch» executes the second expression only when
  ; the first one produces no results: this is the case when
  ; expression under «not» is «true», and accept the frame.
  ;
  ; But we can't return the frame when expression under «not»
  ; has results. For this we always break it with (require #f),
  ; but still remember the passed frame. But we can't just set
  ; the value of «passed» variable as Amb always returns it to
  ; initial state (see «4.3.1/eval-impl-forms.scm»).
  ;
  ; To overcome this limitation we had to implement a new form
  ; «setx!» that behaves as in regular Eval.
  ;
  (amb-catch
   (begin
    (qeval-disp (make-pattern (car operands)) frame)
    (setx! passed frame)
    (require #f)
   )
   (begin
    (let ((passed-copy passed))
     (setx! passed '()) ;<— clean up...
     (require (null? passed-copy))
     frame
    )
   )
  )
 )

 (global qeval-not)

 (define (qeval-value call frame)
  (require (eval-lisp-value call frame))
  frame
 )

 (global qeval-value)
)
