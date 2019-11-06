(include "tree-test-base.scm")
(include "tree-util-walk.scm")

(define (log-walk way)
 (define walker (make-tree-util-walker NumTree))
 (define get (tree-op-get NumTree))

 (define (where w)
  (cond
   ((eq? 'L w) 'left)
   ((eq? 'R w) 'right)
   ((eq? 'U w) 'up)
   ((eq? 'O w) 'root)
   (else (error "Unknown way") w)
  )
 )

 (define (iter node from stack)
  (if (null? way) 'done
   (let* ((w (where (car way))))
    (set! way (cdr way))
    (log "Going " w " from " (get node))
    w ;<— request this walk
   )
  )
 )

 (log "Started walking " way)
 (let ((r (walker sample iter)))
  (cond
   ((eq? 'break (car r))
    (log "Walk breaked on: " (get (caddr r))
     " when moved to: " (cadr r) "\n"
    )
   )

   ((eq? 'result (car r))
    (log "Walk exited on node: " (get (caddr r))
     " with result: " (cadr r) "\n"
    )
   )

   (else (error "Wrong walk result" r))
  )
 )
)

(add 10 5 3 7 1 2 6 15 13 12 0 14 11 17 4)
(log-sample "tree to walk")
(log-walk '(L L L L L))
(log-walk '(L L L L))
(log-walk '(R R R))
(log-walk '(R R))
(log-walk '(R L L L))
(log-walk '())
(log-walk '(U))
(log-walk '(O))
(log-walk '(R U L U R L))
(log-walk '(L R L O R L L L))
