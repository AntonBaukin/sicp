(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.5.2/series.scm")
(include "float-str.scm")

(define (log . args) (for-each display args) (newline))


(define integers (integers-stream 1))

; Series of +1 -1 +1 -1 ...
(define ±1 (cons-stream 1 (cons-stream -1 ±1)))
(log "series of ±1: " (sub-stream->list 10 ±1))

(define integers± (mul-streams integers ±1))
(log "±1 integers: " (sub-stream->list 10 integers±))

; We still able to get rationals...
(define (invert x) (/ 1 x))
(define inverted± (stream-map invert integers±))
(log "±1 inverted integers: " (sub-stream->list 10 inverted±))

(define ln2 (partial-sums inverted±))

(define (log-approx n series)
 (define (next a i tail)
  (define b (inexact (car tail)))
  (define δ (inexact (- b a)))

  (log "@" i "  " (float->str 8 δ) "  " b)

  (if (not (null? (cdr tail)))
   (next b (+ 1 i) (cdr tail))
  )
 )

 (define a (inexact (stream-car series)))

 (log "@0               "  a)
 (next a 1 (sub-stream->list (- n 1) (stream-cdr series)))
)

(log "\nDirect ln2 series: ")
(log-approx 10 ln2)
(log "          ln2 ≈ 0.693147180559945309417232121458176568075500134360255254120")
; On 1000 step we have .6936464315588213 — only three valid digits!
(log "        @1000    " (inexact (stream-ref ln2 1000)))

; For euler-transform — see «series.scm».
(define ln2+ (euler-transform ln2))
(log "\nEuler transform of ln2 series: ")
(log-approx 10 ln2+)
; Got this at step  @9 .6930657506744463 — it's 100 times faster!
(log "          ln2 ≈ 0.693147180559945309417232121458176568075500134360255254120")

(define ln2++ (accelerated-series euler-transform ln2))
(log "\nAccelerated Euler transform of ln2 series: ")
(log-approx 8 ln2++)
; Got this at step  @9 .6931471805599444 — 14 valid digits
(log "          ln2 ≈ 0.693147180559945309417232121458176568075500134360255254120")

; This is possible due to rational arithmetics for all steps, the resulting
; fraction of step 9 has 15,886 digits in both parts! But computing the next
; item takes much, much, much... time longer!
