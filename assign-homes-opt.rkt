#lang racket
(require 
    cpsc411/compiler-lib
    "assign-registers.rkt"
    "undead-analysis.rkt"
    "conflict-analysis.rkt"
    "replace-locations.rkt"
    "uncover-locals.rkt")
(provide
assign-homes-opt)

;; asm-lang-v2 -> nested-asm-lang-v2
;; Replaces each abstract location with a physical location from
;; graph coloring algorithm
(define (assign-homes-opt p)
    (replace-locations 
        (assign-registers 
            (conflict-analysis 
                (undead-analysis 
                    (uncover-locals p))))))
(module+ test
    (require rackunit)
    (check-true
        (equal?
            (assign-homes-opt 
                '(module
                    (begin
                        (set! x.1 0)
                        (halt x.1))))
            `((begin (set! r15 0) (halt r15)))))
    (check-true
        (equal?
            (assign-homes-opt '(module
                (begin
                    (set! x.1 0)
                    (set! y.1 x.1)
                    (set! w.1 1)
                    (set! w.1 (+ w.1 y.1))
                    (halt w.1))))
            '(begin
                (set! r15 0)
                (set! r14 r15)
                (set! r15 1)
                (set! r15 (+ r15 r14))
                (halt r15)))))