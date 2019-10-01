(include "2.3.2-2.5y.scm")
(define (log . args) (for-each display args) (newline))

(define (make-sum-expr expr-a expr-b)
 (cond
  ((=number? 0 expr-a) expr-b)
  ((=number? 0 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (+ expr-a expr-b)
  )

  (else (list expr-a '+ expr-b))
 )
)

(define (sum? expr)
 (and (pair? expr) (eq? '+ (cadr expr)))
)

(define (make-sum expr-a expr-b)
 (cond

  ((sum? expr-a)
   (append expr-a (list '+ expr-b))
  )

  ((sum? expr-b)
   (append (list expr-a '+) expr-b)
  )

  (else (make-sum-expr expr-a expr-b))
 )
)

(define (addend sum-expr)
 (car sum-expr)
)

(define (augend sum-expr)
 (let ((res (cddr sum-expr)))
  (if (null? (cdr res)) (car res) res)
 )
)

(define (make-product-expr expr-a expr-b)
 (cond
  ((=number? 0 expr-a) 0)
  ((=number? 0 expr-b) 0)
  ((=number? 1 expr-a) expr-b)
  ((=number? 1 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (* expr-a expr-b)
  )

  (else (list expr-a '* expr-b))
 )
)

(define (product? expr)
 (and (pair? expr) (eq? '* (cadr expr)))
)

(define (make-product expr-a expr-b)
 (cond

  ((product? expr-a)
   (append expr-a (list '* expr-b))
  )

  ((product? expr-b)
   (append (list expr-a '*) expr-b)
  )

  (else (make-product-expr expr-a expr-b))
 )
)

(define (multiplier product-expr)
 (car product-expr)
)

(define (multiplicand product-expr)
 (let ((res (cddr product-expr)))
  (if (null? (cdr res)) (car res) res)
 )
)

(log "wrong deriv (x + x * x + x) = " (deriv-x '(x + x * x + x)))
(log "that  deriv (x + x * (x + x)) = " (deriv-x '(x + x * (x + x))))
(log "scope deriv (x + ((x * x) + x)) = " (deriv-x '(x + ((x * x) + x))))

; In the given expression list searches for the first
; addition '+ and returns pair of the left and the right
; sides of the expression as a pair. Else, returns '().
;
; In general case, we split by all operators having
; a step down lower priority: '+ and '-.
(define (split-by-sum expr)
 (define (next left right)
  (if (null? (cdr right)) '()
   (let* (
     (a (car right))
     (x (cadr right))

     ;—> the left side is empty or has muls only
     (l (append left (if (null? left) (list a) (list '* a))))
     (r (cddr right)) ;<— skip the sign
    )

    (if (eq? '+ x) (cons l r) (next l r))
   )
  )
 )

 (next '() expr)
)

;(log "split by sum (x * y * z) = "
; (split-by-sum '(x * y * z)))
;
;(log "split by sum (x * y * z + v * w) = "
; (split-by-sum '(x * y * z + v * w)))

(define (sum? expr)
 (and (pair? expr) (not (null? (split-by-sum expr))))
)

;(log "sum? (x * y * z) = " (sum? '(x * y * z)))
;(log "sum? (x * y + z) = " (sum? '(x * y + z)))

; This is a more restrictive predicate, thus the selectors stay as-is!
(define (product? expr)
 (and (pair? expr) (eq? '* (cadr expr))
  (null? (split-by-sum expr))
 )
)

;(log "product? (x * y) = " (product? '(x * y)))
;(log "product? (x + y) = " (product? '(x + y)))
;(log "product? (x * y * z) = " (product? '(x * y * z)))
;(log "product? (x * y + z) = " (product? '(x * y + z)))

(define (addend sum-expr)
 (let ((res (car (split-by-sum sum-expr))))
  (if (null? (cdr res)) (car res) res)
 )
)

;(log "addend (x + y) = " (addend '(x + y)))
;(log "addend (x * y + z) = " (addend '(x * y + z)))

(define (augend sum-expr)
 (let ((res (cdr (split-by-sum sum-expr))))
  (if (null? (cdr res)) (car res) res)
 )
)

;(log "augend (x + y) = " (augend '(x + y)))
;(log "augend (x * y + z) = " (augend '(x * y + z)))
;(log "augend (x + y + z) = " (augend '(x + y + z)))

(log "fixed deriv (x + x * x + x) = " (deriv-x '(x + x * x + x)))
