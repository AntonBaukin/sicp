
(define (make-rb-tree-balance-add)
 (define (make-red-node item)
  (list item 'red '() '())
 )

 (define (get-left node)
  (caddr node)
 )

 (define (set-left node left)
  (set-car! (cddr node) left)
 )

 (define (get-right node)
  (cadddr node)
 )

 (define (set-right node right)
  (set-car! (cdddr node) right)
 )

 (define (si i stack)
  (list-ref stack i)
 )

 (define (get-ileft i stack)
  (get-left (si i stack))
 )

 (define (get-iright i stack)
  (get-right (si i stack))
 )

 ; Is i-th item in stack is a left child of i+1-th?
 (define (ileft? i stack)
  (eq?
   (si i stack)
   (get-ileft (+ i 1) stack)
  )
 )

 (define (iright? i stack)
  (eq?
   (si i stack)
   (get-iright (+ i 1) stack)
  )
 )

 (define (black? node)
  (or
   (null? node) ;<— also treated as black
   (eq? 'black (cadr node))
  )
 )

 (define (ired? i stack)
  (eq? 'red (cadr (si i stack)))
 )

 (define (red-red? stack)
  (and (ired? 0 stack) (ired? 1 stack))
 )

 (define (set-color color node)
  (set-car! (cdr node) color)
 )

 (define (set-red node)
  (set-color 'red node)
 )

 (define (set-ired i stack)
  (set-red (si i stack))
 )

 (define (set-black node)
  (set-color 'black node)
 )

 (define (set-iblack i stack)
  (set-black (si i stack))
 )

 ; Stack is: (target, father, grandpa,..)
 (define (get-uncle stack)
  (if (ileft? 1 stack)
   (get-iright 2 stack)
   (get-ileft 2 stack)
  )
 )

 (define (set-uncle-black stack)
  (set-color 'black (get-uncle stack))
 )

 (define (balance stack)
  (cond
   ; Has only (root)?
   ((null? (cdr stack))
    ; Protect the root from painterd red:
    (set-iblack 0 stack)
    (car stack) ;<— return the root
   )

   ; Has only (red-target root)?
   ((null? (cddr stack))
    (balance (cdr stack))
   )

   ; Colors are valid?
   ((not (red-red? stack))
    (balance (cdr stack))
   )

   ((black? (get-uncle stack))
    (balance-black-uncle stack)
   )

   (else (balance-red-uncle stack))
  )
 )

 (define (balance-red-uncle stack)
  ; Color father and uncle black:
  (set-iblack 1 stack)
  (set-uncle-black stack)

  ; Now the grandpa is colored red, proceed with it:
  (set-ired 2 stack)
  (balance (cddr stack))
 )

 ; We treat 4 cases of left-right positions:
 ; rr, rl, lr, ll, — where code XY means:
 ; XY — «n» is Y of «p», which is X of «g».
 ; Here «n» is 0th, targhet node (it's read),
 ; «p» is 1th, it's father (also red), and
 ; «g» - 2th in the stack, grand-father.
 (define (rr? stack)
  (and (iright? 1 stack) (iright? 0 stack))
 )

 (define (rl? stack)
  (and (iright? 1 stack) (ileft? 0 stack))
 )

 (define (lr? stack)
  (and (ileft? 1 stack) (iright? 0 stack))
 )

 (define (ll? stack)
  (and (ileft? 1 stack) (ileft? 0 stack))
 )

 (define (balance-black-uncle stack)
  (define g (si 2 stack)) ; <— grandpa
  (define p (si 1 stack)) ; <— pa (it's red)
  (define n (si 0 stack)) ; <— target child (it's also red)

  (cond
   ((rr? stack)
    ; Long rotate left g-p-n, color: g — red, p — black:
    (set-gg-child stack)
    (set-right g (get-left p))
    (set-left p g)
    (set-red g)
    (set-black p)
    (balance (cons p (cdddr stack)))
   )

   ((rl? stack)
    ; Short rotate right p-n, do not change colors:
    (set-right g n)
    (set-left p (get-right n))
    (set-right n p)
    ; Swap p-n in the stack and transit to rr:
    (balance (cons p (cons n (cddr stack))))
   )

   ((lr? stack)
    ; Short rotate right p-n, do not change colors:
    (set-left g n)
    (set-right p (get-left n))
    (set-left n p)
    ; Swap p-n in the stack and transit to rr:
    (balance (cons p (cons n (cddr stack))))
   )

   ((ll? stack)
    ; Long rotate right g-p-n, color: g — red, p — black:
    (set-gg-child stack)
    (set-left g (get-right p))
    (set-right p g)
    (set-red g)
    (set-black p)
    (balance (cons p (cdddr stack)))
   )

   (else "Illegal red-black tree balance state!" stack)
  )
 )

 ; Checks that stack has length for (n p g gg ...)
 ; and assigns «p» to be child in position of «g».
 (define (set-gg-child stack)
  (if (< (length stack) 4) void
   ((if (ileft? 2 stack) set-left set-right)
    (si 3 stack) (si 1 stack)
   )
  )
 )

 (define (trace-root node stack)
  (if (null? stack) node
   (trace-root (car stack) (cdr stack))
  )
 )

 (define (add-update cmd item node stack)
  (if (or (eq? 'L cmd) (eq? 'R cmd))
   ; Create read leaf node to append:
   (let ((leaf (make-red-node item)))
    (if (eq? 'L cmd)
     (set-left node leaf)
     (set-right node leaf)
    )

    ; If we added red leaf to black node, we must
    ; simply return the existing root, as RB-rules
    ; are not violated in this case.
    (if (black? node)
     (trace-root node stack)
     (balance (cons leaf (cons node stack)))
    )
   )
   (begin
    (set node item)
    (trace-root node stack)
   )
  )
 )

 add-update ;<— resulting add-update function
)
