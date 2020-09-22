(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")

(eval-basic (debug on))

(eval-basic
 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (define (exclude result items i)
  (cond
   ((null? items) (reverse result))
   ((eq? i (car items))
    (reverse (append (reverse (cdr items)) result))
   )
   (else
    (exclude (cons (car items) result) (cdr items) i)
   )
  )
 )

 ; Builds all combinations of the given items.
 (define (combine result items)
  (if (null? items) (reverse result)
   (let ((i (amb-of items)))
    (combine (cons i result) (exclude '() items i))
   )
  )
 )

 (define (betty tuple) (list-ref tuple 0))
 (define (ethel tuple) (list-ref tuple 1))
 (define (joan tuple) (list-ref tuple 2))
 (define (kitty tuple) (list-ref tuple 3))
 (define (mary tuple) (list-ref tuple 4))

 (define (result tuple)
  (list
   (list 'betty (betty tuple))
   (list 'ethel (ethel tuple))
   (list 'joan (joan tuple))
   (list 'kitty (kitty tuple))
   (list 'mary (mary tuple))
  )
 )

 (define (one-is-true a b)
  (require (or (and a (not b)) (and (not a) b)))
 )

 (define (solve-liars)
  (define t (combine '() '(1 2 3 4 5)))

  (one-is-true
   (eq? 2 (kitty t))
   (eq? 3 (betty t))
  )

  (one-is-true
   (eq? 1 (ethel t))
   (eq? 2 (joan t))
  )

  (one-is-true
   (eq? 3 (joan t))
   (eq? 5 (ethel t))
  )

  (one-is-true
   (eq? 2 (kitty t))
   (eq? 4 (mary t))
  )

  (one-is-true
   (eq? 4 (mary t))
   (eq? 1 (betty t))
  )

  (result t)
 )

 (global solve-liars)
)

(log "——— Solve the problem of Five liars ——— " "\n"
 (eval-amb-results (solve-liars)) "\n"
)
;
; The answer is: (betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4)
;
; To say... When solving this task ahead on a paper,
; I've got instant kill first tracing from Kitty...
;
