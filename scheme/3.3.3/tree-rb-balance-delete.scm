
(define (make-rb-tree-balance-delete)
 (define (get node)
  (car node)
 )

 (define (set node item)
  (set-car! node item)
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

 (define (delete-inner node stack next-node next-stack)
  ; This is the full way up to the root from next node:
  (define full-stack (append next-stack (list node) stack))
  (define l? (sleft? next-node full-stack))

  ; Copy the item of the next node, but leave the color
  ; as-is, because we are going to remove the next.
  (set node (get next-node))

  ; Next node may have only a right child, and we remove
  ; it using previously resolved two cases.
  ((if (null? (get-right next-node)) delete-leaf delete-sole)
   l? next-node full-stack
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
   (else (delete-inner node stack next-node next-stack))
  )
 )

 delete ;<— resulting balance function
)