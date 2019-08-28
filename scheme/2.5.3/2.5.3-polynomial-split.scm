
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

 (define (make-triple var order coeff)
  (cons var (cons order coeff))
 )

 (define (make-triple-term var term)
  (cons var term)
 )

 (define (triple-order triple) (cadr triple))
 (define (triple-coeff triple) (cddr triple))

 ; For three or more levels of nesting polynomials, we
 ; have to deal with the case when coefficient is not
 ; a scalar, or a triple, but a mul-list of triples.

 (define (mul-nested? triple) (eq? '* (car triple)))

 (define (mul-nested var order mul-list res)
  (log " mul-nested " var " o = " order " list: " mul-list )
  (cons
   (append
    (list
     '*
     (make-triple var order 1)
    )
    (cdr mul-list) ;<— remove leading '*
   )
   res
  )
 )

 (define (mul-sum var order sum res)
  (log "mul-sum " var " order = " order " sum: " sum)

  (cond
   ((null? sum) res) ;<— done?

   ; We have sum of terms of inner polynomial with zero power
   ; term (scalar) — we just append all the triples to the result.
   ((= 0 order) (append res sum))

   ((mul-nested? (car sum))
    (mul-sum var order (cdr sum)
     (mul-nested var order (car sum) res)
    )
   )

   ; The next triple of the sum is a scalar: we just take
   ; one's coeff in multiplied triple.
   ((= 0 (triple-order (car sum)))
    (mul-sum var order (cdr sum)
     (cons
      (make-triple var order (triple-coeff (car sum)))
      res
     )
    )
   )

   ; In general case we add a multiplication list:
   (else
    (mul-sum var order (cdr sum)
     (cons
      (list
       '*
       (make-triple var order 1)
       (car sum)
      )
      res
     )
    )
   )
  )
 )

 (define (split-term res var term)
  (let ((sum (split-safe var (cdr term))))
   (if (eq? void sum) ;<— just add scalar term
    (cons (make-triple-term var term) res)
    (mul-sum var (car term) (cdr sum) res)
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