
(define (install-polynomial-split scope)
 (define TAG  'polynomial) ;<— polynomial generic type
 (define TTAG 'sparse)     ;<— type for sparse tags set
 (define STTG (list 'symbol TTAG))


 (define apply-generic (apply-generic-scope-function scope))

 ; Invokes eneric split on the polynomial terms.
 (define (split poly)
  (let ((p (apply-generic-unwrap poly)))
   (apply-generic
    'split
    (apply-generic-tag 'symbol (car p))
    (cdr p) ;<— generic polynomial terms
   )
  )
 )

 ; Splits terms of a polynomial, returns void for scalars.
 (define (split-safe var coeff)
  (if (apply-generic-tagged? TAG coeff)
   (split coeff) void
  )
 )

 ; Creates entry from polynomial term pair (order . coeff).
 ; Entry is a list of ('* coeff (var . order) ... ). Pairs of
 ; (var . order) are added while doing nested multiplication.
 (define (make-entry-from-term var term)
  (if (= 0 (car term))
   (list '* (cdr term))
   (list '* (cdr term) (cons var (car term)))
  )
 )

 (define (make-entry var order coeff)
  (if (= 0 order)
   (list '* coeff)
   (list '* coeff (cons var order))
  )
 )

 ; Adds (var . order) to the given entry.
 (define (mul-entry e var order)
  (if (= 0 order) e ;<— it's x ^ 0
   (append e (list (cons var order)))
  )
 )

 ; Scalar entry has single variable with 0 order.
 (define (scalar-entry? e) (= 2 (length e)))
 (define (scalar-entry-coeff e) (cadr e))

 (define (mul-sum var order sum res)
  (log "mul-sum " var " order = " order " sum: " sum)

  (cond
   ((null? sum) res) ;<— done?

   ; We have sum of terms of inner polynomial with zero power
   ; term (scalar) — we just append all the triples to the result.
   ((= 0 order) (append res sum))

;   ((mul-nested? (car sum))
;    (mul-sum var order (cdr sum)
;     (mul-nested var order (car sum) res)
;    )
;   )

   ; The next entry of the sum is a scalar:
   ; we just take coeff in resulting entry.
   ((scalar-entry? (car sum))
    (mul-sum var order (cdr sum)
     (cons (make-entry var order (scalar-entry-coeff (car sum))) res)
    )
   )

   ; In general case we add (var . order) pair to the entry.
   (else
    (mul-sum var order (cdr sum)
     (cons (mul-entry (car sum) var order) res)
    )
   )
  )
 )

 (define (split-term res var term)
  (let ((sum (split-safe var (cdr term))))
   (if (eq? void sum) ;<— just add scalar term
    (cons (make-entry-from-term var term) res)
    (mul-sum var (car term) sum res)
   )
  )
 )

 (define (split-sparse-terms-iter res var terms)
  (if (null? terms) res
   (split-sparse-terms-iter
    (split-term res var (car terms))
    var (cdr terms)
   )
  )
 )


 ; Register generic spelit for sparse terms:
 ((apply-generic-scope-register scope)
  'split STTG (curry split-sparse-terms-iter '())
 )

 split ;<— resulting function
)