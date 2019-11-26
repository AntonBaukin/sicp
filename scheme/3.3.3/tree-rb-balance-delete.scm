
(define (make-rb-tree-balance-delete . ops)
 (define get-left (list-ref ops 0))
 (define set-left (list-ref ops 1))
 (define get-right (list-ref ops 2))
 (define set-right (list-ref ops 3))


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
    (log "Balance left: red sibling")
    (trace-root stack)
   )

   ((red? r)
    (log "Balance left: red right")
    (copy-color s p)
    (set-black p)
    (set-black r)
    (trace-root (rotate-left stack)) ;<— rotate left and exit
   )

   ((red? l)
    (log "Balance left: red left")
    (set-red s)
    (set-black l)
    (set! stack (rotate-right (cons s stack)))
    (balance-left (cdr stack))
   )

   (else
    (log "Balance left: both black " (car s))
    (set-red s)
    (if (red? p)
     (begin
      (set-black p)
      (trace-root stack)
     )
     (balance-stack stack)
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
    (log "Balance right: red sibling")
    (trace-root stack)
   )

   ((red? l)
    (log "Balance right: red left")
    (copy-color s p)
    (set-black p)
    (set-black l)
    (set! stack (rotate-right stack))
    (trace-root stack) ;<— rotate right and exit
   )

   ((red? r)
    (log "Balance right: red right")
    (set-red s)
    (set-black r)
    (set! stack (rotate-left (cons s stack)))
    (balance-right (cdr stack))
   )

   (else
    (log "Balance right: both black " (car s))
    (set-red s)
    (if (red? p)
     (begin
      (set-black p)
      (trace-root stack)
     )
     (balance-stack stack)
    )
   )
  )
 )

 (define (balance-select stack left?)
  ((if left? balance-left balance-right) stack)
 )

 (define (balance-stack stack)
  ; Only the root is left?
  (if (null? (cdr stack))
   (car stack) ;<— return this root
   (balance-select stack
    (left? (car stack) (cadr stack))
   )
  )
 )

 (define (delete status node stack next-node next-stack)
  ; Balance behaviour alters by left-right position of the
  ; removed node relative to it's parent. Save it ahead:
  (define left? (sleft? node stack))

  (cond
   ((eq? '0 status)
    ; We are free to delete leaf red node:
    (replace-child stack node '())

    (if (red? node)
     (trace-root stack) ;<— simply exit on removing red
     ; But removing black leaf may alter black-length:
     (balance-select stack left?)
    )
   )

   ((or (eq? 'L status) (eq? 'R status))
    ;(replace-child stack node (get-left node))
    (trace-root stack)
   )

   (else
    (trace-root stack)

;    ; As the next node has no left child, we first
;    ; assign it as the left child of the removed one:
;    (set-left next-node (get-left node))
;
;    ; If next node has parent being the deleted node,
;    ; we just replace it. Else, we first detach the
;    ; next node from it's parent, then swap them
;    (if (null? next-stack)
;     (replace-child stack node next-node)
;     (begin
;      (replace-child next-stack next-node (get-right next-node))
;      (set-right next-node (get-right node))
;      (replace-child stack node next-node)
;     )
;    )
   )
  )
 )

 delete ;<— resulting balance function
)