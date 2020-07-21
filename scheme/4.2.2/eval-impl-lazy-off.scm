
;
; Extends and alters «eval-impl-lazy.scm».
;
; Instead of thunking each argument of a compound procedure,
; this file adds two special forms: «lazy» and «lazy-mem».
; They create memoized and not memoized thunks and work
; exactly as «delay» form from SICP Chapter 3.
;
; Unlike «delay», these forms are auto-resolved, and
; call to «force» function is not required.
;

; Here we revert passing argument procedures directly
; into «eval-disp-apply».
(define eval-apply-call-aps-original
 (
  (lambda () ;<— immediately invoked function
   (define (original-aps aps env)
    (map (lambda (ap) (ap env)) aps)
   )

   (set! eval-apply-call-aps original-aps)
   original-aps ;<— resulting function
  )
 )
)


(define thunk-aps-off
 (
  (lambda () ;<— immediately invoked function
   (define (not-thunk-aps aps env) aps)
   (set! thunk-aps not-thunk-aps)
   not-thunk-aps ;<— resulting function
  )
 )
)

(define resolve-aps-strict
 (
  (lambda () ;<— immediately invoked function
   (define (resolve-strict args env)
    (map resolve-value args)
   )

   (set! resolve-aps resolve-strict)
   resolve-strict ;<— resulting function
  )
 )
)

(define eval-lazy-form
 (
  (lambda () ;<— immediately invoked function
   (define (lazy-form exp)
    (define p (eval-analyze (cadr exp)))
    (lambda (env) (make-thunk p env #f))
   )

   (eval-disp-register-form 'lazy lazy-form)
   lazy-form ;<— resulting form
  )
 )
)

(define eval-lazy-mem-form
 (
  (lambda () ;<— immediately invoked function
   (define (lazy-mem-form exp)
    (define p (eval-analyze (cadr exp)))
    (lambda (env) (make-thunk p env #t))
   )

   (eval-disp-register-form 'lazy-mem lazy-mem-form)
   lazy-mem-form ;<— resulting form
  )
 )
)
