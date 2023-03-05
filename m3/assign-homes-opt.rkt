#lang racket
(require cpsc411/compiler-lib
         cpsc411/langs/v4
         "assign-registers.rkt"
         "undead-analysis.rkt"
         "conflict-analysis.rkt"
         "../m2/replace-locations.rkt"
         "../m2/uncover-locals.rkt")
(provide assign-homes-opt)

;; Milestone 3 Exercise 4
;;
;; Replaces each abstract location with a physical location from
;; graph coloring algorithm
(define (assign-homes-opt p)
  (-> asm-pred-lang-v4? nested-asm-lang-v4?)
  (replace-locations (assign-registers (conflict-analysis (undead-analysis (uncover-locals p))))))

(module+ test
  (require rackunit)
  (check-equal? (assign-homes-opt '(module ()
                                           (begin
                                             (set! x.1 0)
                                             (halt x.1))))
                `(begin
                   (set! r15 0)
                   (halt r15)))
  (check-equal? (assign-homes-opt '(module ()
                                           (begin
                                             (set! x.1 0)
                                             (set! y.1 x.1)
                                             (set! w.1 1)
                                             (set! w.1 (+ w.1 y.1))
                                             (halt w.1))))
                '(begin
                   (set! r15 0)
                   (set! r15 r15)
                   (set! r14 1)
                   (set! r14 (+ r14 r15))
                   (halt r14))))
