
; Split is implemented for sparse polynomials in a generic way.
; Taking terms like 10x² + ((3z² + 5z)y² + (7z + 9)y + 2)x, it
; produces a sum-list of mul-entries being a lists of form:
; ('* coeff ('var-x . order-x) ('var-y . order-y)...) with
; arbitrary variables. The var-order pairs are sorted by
; the variable name. Entries in the list are sorted by
; the sum order descending. They are also merged: only
; one entry with vars-order combination exists.
;
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

 ; Compares «var» with pair's (var . order) variable by the name.
 (define (var<? a b)
  (string-ci<? (symbol->string a) (symbol->string b))
 )

 ; Adds (var . order) to the given entry.
 (define (mul-entry-iter var order vos res)
  (cond
   ((null? vos)
    (append res (list (cons var order)))
   )

   ; Found entry with the same variable?
   ((eq? var (caar vos))
    (append
     res
     (list (cons var (+ (cdar vos) order)))
     (cdr vos)
    )
   )

   ; This forms the list of (var . order) pairs
   ; of an entry sorted by the variable name
   ; ascending, as ('x . ?) ('y . ?) ('z . ?)...
   ((var<? var (caar vos))
    (append res (list (cons var order)) vos)
   )

   (else
    (mul-entry-iter var order (cdr vos)
     (append res (list (car vos)))
    )
   )
  )
 )

 (define (mul-entry e var order)
  (if (= 0 order) e ;<— it's x ^ 0
   (mul-entry-iter var order (cddr e)
    (list '* (cadr e)) ;<— ('* coeff)
   )
  )
 )

 ; Scalar entry has single variable with 0 order.
 (define (scalar-entry? e) (= 2 (length e)))
 (define (scalar-entry-coeff e) (cadr e))

 (define (mul-sum var order sum res)
  (cond
   ((null? sum) res) ;<— done?

   ; We have sum of terms of inner polynomial with zero power
   ; term (scalar) — we just append all the triples to the result.
   ((= 0 order) (append res sum))

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

 ; Sum of orders of (var . order) pairs list.
 (define (vos-order-sum vos)
  (if (null? vos) 0
   (+ (cdar vos) (vos-order-sum (cdr vos)))
  )
 )

 ; Compares two lists of (var . order) pairs
 ; of equals lengths and sum orders. See entry<?().
 (define (vos<? vos-a vos-b)
  (if (and (null? vos-a) (null? vos-b)) #f ;<— they are equal
   (let (
     (soa (vos-order-sum vos-a))
     (sob (vos-order-sum vos-b))
    )

    (cond
     ((< soa sob) #f)
     ((> soa sob) #t)
     ((< (cdar vos-a) (cdar vos-b)) #f)
     ((> (cdar vos-a) (cdar vos-b)) #t)
     (else (vos<? (cdr vos-a) (cdr vos-b)))
    )
   )
  )
 )

 ; Compares two entries. Note that (var . order) pairs
 ; are already sorted by variable name ascending.
 ; Longer entries come first.
 (define (entry<? a b)
  (let (
    (la (length a))
    (lb (length b))
   )

   (cond
    ((< la lb) #f) ;<— length descending
    ((< lb la) #t)
    ; We skip two leading ('* coeff ...)
    (else (vos<? (cddr a) (cddr b)))
   )
  )
 )

 ; Checks that two lists of (var . order) pairs are same.
 ; Takes entries of ('* coeff ... (var . order) ...).
 (define (entry-vos-eq? a b)
  (equal? (cddr a) (cddr b)) ;<— just the same lists, deeply
 )

 ; We sum coefficients of two same entries.
 (define (sum-same-entries a b)
  (list
   '*
   (add (cadr a) (cadr b))
   (cddr a) ;<— same list as (cddr b)
  )
 )

 ; Takes sorted list of entries and returns reversed
 ; list of merged same neighbour ones.
 (define (merge-entries-iter entries res)
  (cond
   ((null? entries) res)

   ((null? (cdr entries))
    (cons (car entries) res)
   )

   ((entry-vos-eq? (car entries) (cadr entries))
    (merge-entries-iter (cddr entries)
     (cons (sum-same-entries (car entries) (cadr entries)) res)
    )
   )

   (else
    (merge-entries-iter (cdr entries)
     (cons (car entries) res)
    )
   )
  )
 )

 (define (merge-entries entries)
  (reverse
   (merge-entries-iter
    (quick-sort entry<? entries)
    '()
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

 (define (split-sparse-terms var terms)
  (merge-entries (split-sparse-terms-iter '() var terms))
 )

 ; Register generic spelit for sparse terms:
 ((apply-generic-scope-register scope)
  'split STTG split-sparse-terms
 )

 ; Resulting functions:
 (list split merge-entries)
)
