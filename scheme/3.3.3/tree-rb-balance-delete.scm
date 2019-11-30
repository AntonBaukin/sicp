
(define (make-rb-tree-balance-delete . ops)
 (define get-left (list-ref ops 0))
 (define set-left (list-ref ops 1))
 (define get-right (list-ref ops 2))
 (define set-right (list-ref ops 3))


 (define (get-same left? node)
  ((if left? get-left get-right) node)
 )

 (define (left? node parent)
  (eq? node (get-left parent))
 )

 (define (sleft? node stack)
  (if (null? stack) void (left? node (car stack)))
 )

 (define (black? node)
  (or
   (null? node) ;<— also treated as black
   (eq? 'black (cadr node))
  )
 )

 (define (red? node)
  (and
   (not (null? node))
   (eq? 'red (cadr node))
  )
 )

 (define (set-color color node)
  (set-car! (cdr node) color)
 )

 (define (copy-color target source)
  (set-car! (cdr target) (cadr source))
 )

 (define (set-red node)
  (set-color 'red node)
 )

 (define (set-black node)
  (if (null? node) void
   (set-color 'black node)
  )
 )

 (define (replace-child stack child with)
  (cond
   ((null? stack) void)

   ((eq? child (get-left (car stack)))
    (set-left (car stack) with)
   )

   (else
    (set-right (car stack) with)
   )
  )
 )

 (define (trace-root stack)
  (cond
   ((null? stack) '())
   ((null? (cdr stack)) (car stack))
   (else (trace-root (cdr stack)))
  )
 )

 ; Checks that stack has length for (p g),
 ; and replaces «p» with «n». Returns a new
 ; stack having «n» instead of «p».
 (define (set-g-child n stack)
  (define p (car stack))

  (if (null? (cdr stack))
   (list n) ;<— no grandfather
   (let ((g (cadr stack)))
    (if (eq? p (get-left g))
     (set-left g n)
     (set-right g n)
    )
    (cons n (cdr stack))
   )
  )
 )

 ; Rotates left p-n where «n» is right child
 ; of «p» being 0th item of stack.
 (define (rotate-left stack)
  (define p (car stack))
  (define n (get-right p))

  (set-right p (get-left n))
  (set-left n p)
  (set-g-child n stack)
 )

 ; Rotates right n-p where «n» is left child
 ; of «p» being 0th item of stack.
 (define (rotate-right stack)
  (define p (car stack))
  (define n (get-left p))

  (set-left p (get-right n))
  (set-right n p)
  (set-g-child n stack)
 )

 (define (balance-left stack)
  (define p (car stack))
  (define s (get-right p))
  (define l (get-left s))
  (define r (get-right s))

  (cond
   ((red? s)
    ;(ilog "Balance left: red sibling")
    (set-red p)
    (set-black s)
    (set! stack (rotate-left stack))
    (balance-left (cons p stack))
   )

   ((red? r)
    ;(ilog "Balance left: red right")
    (copy-color s p)
    (set-black p)
    (set-black r)
    (trace-root (rotate-left stack)) ;<— rotate left and exit
   )

   ((red? l)
    ;(ilog "Balance left: red left")
    (set-red s)
    (set-black l)
    (set! stack (rotate-right (cons s stack)))
    (balance-select #t (cdr stack))
   )

   (else
    ;(ilog "Balance left: both black " (car s))
    (set-red s)
    (if (red? p)
     (begin
      (set-black p)
      (trace-root stack)
     )
     (balance-select void stack)
    )
   )
  )
 )

 (define (balance-right stack)
  (define p (car stack))
  (define s (get-left p))
  (define l (get-left s))
  (define r (get-right s))

  (cond
   ((red? s)
    ;(ilog "Balance right: red sibling")
    (set-red p)
    (set-black s)
    (set! stack (rotate-right stack))
    (balance-right (cons p stack))
   )

   ((red? l)
    ;(ilog "Balance right: red left")
    (copy-color s p)
    (set-black p)
    (set-black l)
    (set! stack (rotate-right stack))
    (trace-root stack) ;<— rotate right and exit
   )

   ((red? r)
    ;(ilog "Balance right: red right")
    (set-red s)
    (set-black r)
    (set! stack (rotate-left (cons s stack)))
    (balance-select #f (cdr stack))
   )

   (else
    ;(ilog "Balance right: both black " (car s))
    (set-red s)
    (if (red? p)
     (begin
      (set-black p)
      (trace-root stack)
     )
     (balance-select void stack)
    )
   )
  )
 )

 (define (balance-select l? stack)
  ;(ilog "Balance select " l? " " (map car stack))
  (cond
   ((null? stack) '())
   ;((null? (cdr stack)) (car stack))
   (else
    (if (eq? void l?)
     (begin 
      (set! l? (left? (car stack) (cadr stack)))
      (set! stack (cdr stack))
     )
     void
    )

    ((if l? balance-left balance-right) stack)
   )
  )
 )

 (define (delete-leaf l? node stack)
  (replace-child stack node '())

  (if (red? node)
   ; We are free to delete leaf red node:
   (trace-root stack)
   ; But removing black leaf may alter black-length:
   (balance-select l? stack)
  )
 )

 (define (delete-sole l? node stack)
  (define child
   (if (null? (get-left node))
    (get-right node) (get-left node)
   )
  )

  (replace-child stack node child)
  
  (cond
   ; Simply exit on removing red?
   ((red? node) (trace-root stack))

   ; Child to replace removed black is red?
   ((red? child)
    (set-black child)    ;<— just color it black
    (trace-root (cons child stack)) ;<— and exit
   )

   ; So, we have double-black node, and balance up...
   (else (balance-select l? stack))
  )
 )

 (define (delete-inner l? node stack next-node next-stack)
  (define next-right (get-right next-node))

  ; As the next node has no left child, we first
  ; assign it as the left child of the removed one:
  (set-left next-node (get-left node))

  ; If next node has parent being the deleted node,
  ; we just replace it. Else, we first detach the
  ; next node from it's parent, then swap them
  ; Next node is a child of deleted one?
  (if (null? next-stack)
   ; We just replace deleted with it's child:
   (replace-child stack node next-node)
   (begin
    ; We first detach the next node from it's parent:
    (replace-child next-stack next-node next-right)
    ; Attach right branch to the next node:
    (set-right next-node (get-right node))
    ; And attach next to the new parent:
    (replace-child stack node next-node)
   )
  )

  ;(ilog "Color node: " (cadr node) " next: " (cadr next-node))

  ; Now treat 4 variants of removed and next colors.
  ; When both nodes are red, nothing can happen.
  ; When next node is red (removed is black), we
  ; just color next to black...
  (if (red? next-node)
   (begin
    (if (red? node) void (set-black next-node))
    (trace-root stack)
   )
   ; When removed node is black, next one is,
   ; or both, we need to balance-up.
   (balance-select
    ; We always balance on right child, if has it:
    void ; (if (null? next-stack) l? #f)
    ; Next node is [right] child of removed?
    (if (null? next-stack)
     ; Balance stack starts with next node that
     ; replaced the removed one:
     (cons next-node stack)
     (append
      ; Start balance from initial right child of next node:
      (if (null? next-right) '() (list next-right))
      ; Way up from next to the replaced:
      next-stack
      ; Next node that replaced the removed:
      (list next-node)
      ; This is the way up to the root:
      stack
     )
    )
   )
  )
 )

 (define (delete status node stack next-node next-stack)
  ; Balance behaviour alters by left-right position of the
  ; removed node relative to it's parent. Save it ahead:
  (define l? (sleft? node stack))

  (cond
   ; Removed node is a leaf?
   ((eq? '0 status)
    (delete-leaf l? node stack)
   )

   ; Removed node has only one child?
   ((or (eq? 'L status) (eq? 'R status))
    (delete-sole l? node stack)
   )

   ; Remove node having both children:
   (else (delete-inner l? node stack next-node next-stack))
  )
 )

 delete ;<— resulting balance function
)