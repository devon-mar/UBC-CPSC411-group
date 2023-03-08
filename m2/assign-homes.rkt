#lang racket

(require
  cpsc411/langs/v4

  "replace-locations.rkt"
  "assign-fvars.rkt"
  "uncover-locals.rkt")

(provide assign-homes)

;; Milestone 2 Exercise 5
;; Compiles Asm-lang v2 to Nested-asm-lang v2,
;; replacing each abstract location with a physical location.
(define/contract (assign-homes p)
  (-> asm-pred-lang-v4? nested-asm-lang-v4?)

  (replace-locations (assign-fvars (uncover-locals p))))

(module+ test
  (require rackunit)


  (define-check (check-42 p)
    (check-equal?
      (interp-nested-asm-lang-v4 (assign-homes p))
      42))

  ; simple
  (check-42 '(module () (halt 42)))

  ; binop and set!
  (check-42
    '(module
       ()
       (begin
         (set! x.1 38)
         (set! x.1 (+ x.1 2))
         (set! y.1 2)
         (set! y.1 (+ y.1 x.1))
         (halt y.1))))

  ; nested begins
  (check-42
    '(module
       ()
       (begin
         (set! x.1 38)
         (begin
           (set! x.1 (+ x.1 2))
           (set! y.1 2)
           (set! y.1 (+ y.1 x.1)))
         (halt y.1)))))

