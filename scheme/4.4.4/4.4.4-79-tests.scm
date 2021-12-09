(add-rule (boss-ben? ?name)
 (and
  ; (debug frame ">> boss-ben? :: ")
  (supervisor ?name (Bitdiddle Ben))
  ; (debug frame "<< boss-ben? :: ")
 )
)

(test-query
 (and
  (job ?person (computer . ?position))
  ; (debug frame ":: person computer :: ")
  (boss-ben? ?person)
 )
; —————————————————————————————————————————————————————————
 (and (job (Tweakit Lem E) (computer technician)) (boss-ben? (Tweakit Lem E)))
 (and (job (Fect Cy D) (computer programmer)) (boss-ben? (Fect Cy D)))
 (and (job (Hacker Alyssa P) (computer programmer)) (boss-ben? (Hacker Alyssa P)))
)

(test-query
 (and
  (job ?name (computer . ?position))
  (boss-ben? ?name)
 )
; —————————————————————————————————————————————————————————
 (and (job (Tweakit Lem E) (computer technician)) (boss-ben? (Tweakit Lem E)))
 (and (job (Fect Cy D) (computer programmer)) (boss-ben? (Fect Cy D)))
 (and (job (Hacker Alyssa P) (computer programmer)) (boss-ben? (Hacker Alyssa P)))
)

(define (any-<? a b)
 (cond
  ((and (string? a) (string? b))
   (string-ci<? a b)
  )

  ((and (symbol? a) (symbol? b))
   (string-ci<? (symbol->string a) (symbol->string b))
  )

  ((and (number? a) (number? b))
   (< a b)
  )

  (else #f)
 )
)

(define (lists-<? a b)
 (cond
  ((and (null? a) (null? b) #t))
  ((or (not (list? a)) (not (list? b))) #f)
  ((or (null? a) (null? b)) #f)
  ((not (any-<? (car a) (car b))) #f)
  (else (lists-<? (cdr a) (cdr b)))
 )
)

(add-rule (same-boss ?person ?colleague ?position)
 (and
  ; (debug frame ">> same-boss :: ")
  (supervisor ?person ?name)
  (supervisor ?colleague ?name)
  (job ?colleague ?position)
  (lisp-value lists-<? ?person ?colleague)
  ; (debug frame "<< same-boss :: ")
 )
)

(add-rule (same-boss-computer ?name ?colleague)
 (and
  (job ?name (computer . ?position))
  ; (debug frame ":: person computer :: ")
  (same-boss ?name ?colleague ?coposition)
 )
)

(test-query
 (same-boss-computer ?person ?colleague)
; —————————————————————————————————————————————————————————
 (same-boss-computer (Fect Cy D) (Tweakit Lem E))
 (same-boss-computer (Bitdiddle Ben) (Scrooge Eben))
)

; Sample logging:
;
; >> same-boss :: frame ((position (? . coposition) 1) (person (Fect Cy D)) ⏎
;    (colleague (? . colleague) 1)) 2 (frame ((position (programmer)) ⏎
;    (name (Fect Cy D)) (colleague (? . colleague) 0)) 1 ⏎
;    (frame ((person (Fect Cy D))) 0 ()))
;
; << same-boss :: frame ((person (Fect Cy D)) (colleague (Tweakit Lem E)) ⏎
;    (name (Bitdiddle Ben)) (position (computer technician))) 2 ⏎
;    (frame ((coposition (computer technician)) (colleague (Tweakit Lem E)) ⏎
;    (position (programmer)) (name (Fect Cy D))) 1 ⏎
;    (frame ((colleague (Tweakit Lem E)) (person (Fect Cy D))) 0 ()))
;
