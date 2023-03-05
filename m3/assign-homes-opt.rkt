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

  (check-equal?
    (assign-homes-opt
      '(module ()
        (begin
          (set! x.1 0)
          (halt x.1))))
    `(module
      (begin
        (set! r15 0)
        (halt r15))))

  ;; sets
  (check-equal?
    (assign-homes-opt
      '(module ()
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (set! w.1 1)
          (set! w.1 (+ w.1 y.1))
          (halt w.1))))
    '(module
      (begin
        (set! r15 0)
        (set! r15 r15)
        (set! r14 1)
        (set! r14 (+ r14 r15))
        (halt r14))))

  ;; if and pred
  (check-match
    (assign-homes-opt
      '(module ()
        (begin
          (set! u.1 9)
          (set! x.1 u.1)
          (begin
            (if (= x.1 2)
                (set! z.1 9)
                (set! y.1 x.1)))
          (if (begin (set! j.1 17) (set! j.1 (+ j.1 z.1)) (set! i.1 y.1) (= j.1 y.1))
              (halt x.1)
              (halt i.1)))))
    `(module
      (begin
        (set! ,r0 9)
        (set! ,r1 ,r0)
        (begin (if (= ,r1 2) (set! ,r3 9) (set! ,r2 ,r1)))
        (if (begin (set! ,r5 17) (set! ,r5 (+ ,r5 ,r3)) (set! ,r4 ,r2) (= ,r5 ,r2))
            (halt ,r1)
            (halt ,r4))))
    (and (andmap register? (list r0 r1 r2 r3 r4 r5))
         (not (member r0 '(r2 r3)))
         (not (member r1 '(r2 r3 r4 r5)))
         (not (member r2 '(r3 r5)))
         (not (equal? r3 r5))
         (not (equal? r4 r5))))
  )
