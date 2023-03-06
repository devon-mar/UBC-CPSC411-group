#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  "../utils/compiler-utils.rkt")

(provide replace-locations)

;; Milestone 2 Exercise 8
;; Milestone 4 Exercise 12
;;
;; Compiles Asm-lang v4/assignments to Nested-asm-lang v2, replaced each abstract
;; location with its assigned physical location from the assignment info field.
(define/contract (replace-locations p)
  (-> asm-pred-lang-v4/assignments? nested-asm-lang-v4?)

  (define assignments?
    (listof (cons/c aloc? any/c)))

  ; Returns the assignment in a for aloc.
  (define/contract (assignment-ref a aloc)
    (-> assignments? aloc? any/c)
    (car (dict-ref a aloc)))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-pred-lang-v4/assignments-triv
  ;; -> nested-asm-lang-v4-triv
  (define/contract (replace-locations-triv a t)
    (-> assignments? any/c any/c)
    (match t
      [int64 #:when (int64? int64) int64]
      [aloc (assignment-ref a aloc)]))
  
  ;; Replaces each aloc in p with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; p: asm-pred-lang-v4/assignments-pred
  ;; -> nested-asm-lang-v4-pred
  (define/contract (replace-locations-pred a p)
    (-> assignments? any/c any/c)
    (match p
      [`(true) p]
      [`(false) p]
      [`(not ,pred) `(not ,(replace-locations-pred a pred))]
      [`(begin ,effects ... ,pred)
       `(begin
         ,@(map (lambda (e) (replace-locations-effect a e)) effects)
         ,(replace-locations-pred a pred))]
      [`(if ,ppred ,pred1 ,pred2)
       `(if ,(replace-locations-pred a ppred)
            ,(replace-locations-pred a pred1)
            ,(replace-locations-pred a pred2))]
      [`(,relop ,aloc ,triv)
       #:when(relop? relop)
       `(,relop ,(assignment-ref a aloc) ,(replace-locations-triv a triv))]))

  ;; Replaces each aloc in e with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; e: asm-pred-lang-v4/assignments-effect
  ;; -> nested-asm-lang-v4-effect
  (define/contract (replace-locations-effect a e)
    (-> assignments? any/c any/c)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv))
        `(set!
           ,(assignment-ref a aloc)
           (,binop ,(assignment-ref a aloc)
                   ,(replace-locations-triv a triv)))]
      [`(set! ,aloc ,triv)
        `(set! ,(assignment-ref a aloc) ,(replace-locations-triv a triv))]
      ;; removed tail `e` from template
      [`(begin ,es ...)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(replace-locations-pred a pred)
            ,(replace-locations-effect a effect1)
            ,(replace-locations-effect a effect2))]))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-pred-lang-v4/assignments-tail
  ;; -> nested-asm-lang-v4-tail
  (define/contract (replace-locations-tail a t)
    (-> assignments? any/c any/c)
    (match t
      [`(halt ,triv) `(halt ,(replace-locations-triv a triv))]
      [`(begin ,es ... ,tail)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es)
           ,(replace-locations-tail a tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(replace-locations-pred a pred)
            ,(replace-locations-tail a tail1)
            ,(replace-locations-tail a tail2))]))

  ;; Replaces each aloc in t with its assigned physical location from the assignments
  ;; in p's info.
  ;; asm-pred-lang-v4/assignments-p -> nested-asm-lang-v4-p
  (define (replace-locations-p p)
    (match p
      [`(module ,info ,tail)
       `(module ,(replace-locations-tail (info-ref info 'assignment) tail))]))

  (replace-locations-p p))

(module+ test
  (require rackunit)

  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax))))
        (begin
          (set! x.1 0)
          (halt x.1))))
    '(module (begin (set! rax 0) (halt rax))))
  (check-equal?
    (replace-locations
      '(module ((locals (x.1 y.1 w.1))
                (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (set! w.1 (+ w.1 y.1))
          (halt w.1))))
    '(module (begin (set! rax 0) (set! rbx rax) (set! r9 (+ r9 rbx)) (halt r9))))

  ;; nested begins
  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax))))
        (begin
          (begin
            (begin
              (set! x.1 0)
              (set! x.1 42)))
          (halt x.1))))
    '(module
      (begin
        (begin
          (begin
            (set! rax 0)
            (set! rax 42)))
        (halt rax))))

  ;; replace in ifs 
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3 x.4))
         (assignment ((x.1 rdx) (x.2 r12) (x.3 fv0) (x.4 fv7))))
        (begin
          (set! x.1 5)
          (set! x.3 x.1)
          (if (= x.1 2)
              (set! x.2 9)
              (begin
                (set! x.4 1)
                (if (> x.3 x.2)
                    (set! x.3 (* x.3 x.2))
                    (set! x.2 (* x.2 x.3)))))
          (if (if (= x.3 x.2) (not (< x.3 2)) (>= x.4 9))
              (halt x.1)
              (halt x.4)))))
    '(module
      (begin
        (set! rdx 5)
        (set! fv0 rdx)
        (if (= rdx 2)
            (set! r12 9)
            (begin
              (set! fv7 1)
              (if (> fv0 r12)
                  (set! fv0 (* fv0 r12))
                  (set! r12 (* r12 fv0)))))
        (if (if (= fv0 r12) (not (< fv0 2)) (>= fv7 9))
            (halt rdx)
            (halt fv7)))))

  ;; replace in preds
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3 x.4 x.5))
         (assignment ((x.1 rdx) (x.2 r12) (x.3 fv0) (x.4 fv7) (x.5 fv5))))
        (begin
          (set! x.1 5)
          (set! x.3 x.1)
          (if (if (true) (= x.1 2) (not (not (!= x.3 9))))
              (set! x.2 9)
              (if (begin (> x.3 x.2))
                  (set! x.3 (* x.3 x.2))
                  (set! x.2 (* x.2 x.3))))
          (if (begin
                (begin (set! x.5 20))
                (set! x.2 x.4)
                (if (<= x.2 9) (set! x.4 9) (set! x.4 (+ x.4 x.4)))
                (if (false) (not (< x.3 2)) (false)))
              (halt x.1)
              (halt x.4)))))
    '(module
      (begin
        (set! rdx 5)
        (set! fv0 rdx)
        (if (if (true) (= rdx 2) (not (not (!= fv0 9))))
            (set! r12 9)
            (if (begin (> fv0 r12))
                (set! fv0 (* fv0 r12))
                (set! r12 (* r12 fv0))))
        (if (begin
              (begin (set! fv5 20))
              (set! r12 fv7)
              (if (<= r12 9) (set! fv7 9) (set! fv7 (+ fv7 fv7)))
              (if (false) (not (< fv0 2)) (false)))
            (halt rdx)
            (halt fv7)))))
  )
