(include "iife.scm")

; List of connector operations. Connector links
; (mostly two) Constraint instances.
;
; Constraint instance is a tagged list of:
; (tag on-value on-reset ...), where on-* callbacks
; have single argument: (constraint), and «...» —
; means other fields on constraint instance.
;
; Hint: as on-* callbacks do recieve constraint
; instance, they are not bound to it.
;
(define Connector ((lambda ()
 (define TAG 'connector)

 (define (make)
  ; 0 tag, 1 value, 2 informant, 3 constraints
  (list TAG void void '())
 )

 (define (connector? c)
  (and (pair? c) (eq? TAG (car c)))
 )

 (define (check c)
  (if (connector? c) c
   (error "Not a connector instance!" c)
  )
 )

 (define (get-value c)
  (cadr (check c))
 )

 (define (has-value? c)
  (not (eq? void (get-value c)))
 )

 (define (set-value c value)
  (set-car! (cdr c) value)
 )

 (define (get-informant c)
  (caddr c)
 )

 (define (set-informant c informant)
  (set-car! (cddr c) informant)
 )

 (define (get-constraints c)
  (cadddr c)
 )

 (define (add-constraint c co)
  (define cos (get-constraints c))
  (if (not (memq co cos))
   (set-car! (cdddr c) (cons co cos))
  )
 )

 (define (notify-constraints connector except tail)
  (if (not (null? tail))
   (begin
    (if (not (eq? except (car tail)))
     (notify-constraint (car tail) connector)
    )
    (notify-constraints connector except (cdr tail))
   )
  )
 )

 (define (same-value? a b)
  (or
   (and (number? a) (number? b) (= a b))
   (eq? a b)
  )
 )

 (define (assign-value c value informant)
  (cond
   ((not (has-value? c))
    (set-value c value)
    (set-informant c informant)
    (notify-constraints c informant (get-constraints c))
   )

   ((not (same-value? value (get-value c)))
    (error "Assigning connector without reset!" value (get-value c))
   )

   (else void)
  )
 )

 (define (reset-value connector retractor)
  (if (eq? retractor (get-informant connector))
   (begin
    (set-value connector void)
    (set-informant connector void)

    (for-each
     (lambda (c) (reset-constraint c connector))
     (get-constraints connector)
    )
   )
  )
 )

 (define (connect connector constraint)
  (add-constraint (check connector) constraint)

  (if (has-value? connector)
   (notify-constraint constraint connector)
  )
 )

 (list ;<— Connector «class» methods
  make          ; 0
  connector?    ; 1
  has-value?    ; 2
  get-value     ; 3
  assign-value  ; 4
  reset-value   ; 5
  connect       ; 6
 )
)))

(define (constraint? c)
 (and
  (pair? c)
  (>= (length c) 3)
  (symbol? (car c))
  (procedure? (cadr c))
  (procedure? (caddr c))
 )
)

(define (notify-constraint constraint connector)
 (if (constraint? constraint)
  ((cadr constraint) connector)
  (error "Notify not a constraint!" constraint)
 )
)

(define (reset-constraint c connector)
 (if (constraint? c)
  ((caddr c) connector)
  (error "Reset not a constraint!" c)
 )
)

(define make-connector (list-ref Connector 0))
(define connector? (list-ref Connector 1))
(define connector-has-value? (list-ref Connector 2))
(define connector-get-value (list-ref Connector 3))
(define connector-set-value (list-ref Connector 4))
(define connector-reset (list-ref Connector 5))
(define connect (list-ref Connector 6))

(define (connect-each constraint . connectors)
 (for-each (lambda (c) (connect c constraint)) connectors)
)

(define (reset-connectors retractor . connectors)
 (for-each (lambda (c) (connector-reset c retractor)) connectors)
)
